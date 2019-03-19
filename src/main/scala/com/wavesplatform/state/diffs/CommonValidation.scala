package com.wavesplatform.state.diffs

import cats._
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.ValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{Order, _}
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.smart.script.{ContractScript, Script}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._

import scala.util.{Left, Right, Try}

object CommonValidation {

  val ScriptExtraFee = 400000L
  val FeeUnit        = 100000

  val FeeConstants: Map[Byte, Long] = Map(
    GenesisTransaction.typeId            -> 0,
    PaymentTransaction.typeId            -> 1,
    IssueTransaction.typeId              -> 1000,
    ReissueTransaction.typeId            -> 1000,
    BurnTransaction.typeId               -> 1,
    TransferTransaction.typeId           -> 1,
    MassTransferTransaction.typeId       -> 1,
    LeaseTransaction.typeId              -> 1,
    LeaseCancelTransaction.typeId        -> 1,
    ExchangeTransaction.typeId           -> 3,
    CreateAliasTransaction.typeId        -> 1,
    DataTransaction.typeId               -> 1,
    SetScriptTransaction.typeId          -> 10,
    SponsorFeeTransaction.typeId         -> 1000,
    SetAssetScriptTransaction.typeId     -> (1000 - 4),
    smart.InvokeScriptTransaction.typeId -> 5
  )

  def disallowSendingGreaterThanBalance[T <: Transaction](blockchain: Blockchain,
                                                          settings: FunctionalitySettings,
                                                          blockTime: Long,
                                                          tx: T): Either[ValidationError, T] =
    if (blockTime >= settings.allowTemporaryNegativeUntil) {
      def checkTransfer(sender: Address, assetId: Asset, amount: Long, feeAssetId: Asset, feeAmount: Long) = {
        val amountDiff = assetId match {
          case aid @ IssuedAsset(_) => Portfolio(0, LeaseBalance.empty, Map(aid -> -amount))
          case Waves                => Portfolio(-amount, LeaseBalance.empty, Map.empty)
        }
        val feeDiff = feeAssetId match {
          case aid @ IssuedAsset(_) => Portfolio(0, LeaseBalance.empty, Map(aid -> -feeAmount))
          case Waves                => Portfolio(-feeAmount, LeaseBalance.empty, Map.empty)
        }

        val spendings       = Monoid.combine(amountDiff, feeDiff)
        val oldWavesBalance = blockchain.balance(sender, Waves)

        val newWavesBalance = oldWavesBalance + spendings.balance
        if (newWavesBalance < 0) {
          Left(
            GenericError(
              "Attempt to transfer unavailable funds: Transaction application leads to " +
                s"negative waves balance to (at least) temporary negative state, current balance equals $oldWavesBalance, " +
                s"spends equals ${spendings.balance}, result is $newWavesBalance"))
        } else {
          val balanceError = spendings.assets.collectFirst {
            case (aid, delta) if delta < 0 && blockchain.balance(sender, aid) + delta < 0 =>
              val availableBalance = blockchain.balance(sender, aid)
              GenericError(
                "Attempt to transfer unavailable funds: Transaction application leads to negative asset " +
                  s"'$aid' balance to (at least) temporary negative state, current balance is $availableBalance, " +
                  s"spends equals $delta, result is ${availableBalance + delta}")
          }
          balanceError.fold[Either[ValidationError, T]](Right(tx))(Left(_))
        }
      }

      tx match {
        case ptx: PaymentTransaction if blockchain.balance(ptx.sender, Waves) < (ptx.amount + ptx.fee) =>
          Left(
            GenericError(
              "Attempt to pay unavailable funds: balance " +
                s"${blockchain.balance(ptx.sender, Waves)} is less than ${ptx.amount + ptx.fee}"))
        case ttx: TransferTransaction     => checkTransfer(ttx.sender, ttx.assetId, ttx.amount, ttx.feeAssetId, ttx.fee)
        case mtx: MassTransferTransaction => checkTransfer(mtx.sender, mtx.assetId, mtx.transfers.map(_.amount).sum, Waves, mtx.fee)
        case citx: InvokeScriptTransaction =>
          citx.payment.map(p => checkTransfer(citx.sender, p.assetId, p.amount, citx.feeAssetId, citx.fee)).find(_.isLeft).getOrElse(Right(tx))
        case _ => Right(tx)
      }
    } else Right(tx)

  def disallowDuplicateIds[T <: Transaction](blockchain: Blockchain,
                                             settings: FunctionalitySettings,
                                             height: Int,
                                             tx: T): Either[ValidationError, T] = tx match {
    case _: PaymentTransaction => Right(tx)
    case _ =>
      val id = tx.id()
      Either.cond(!blockchain.containsTransaction(tx), tx, AlreadyInTheState(id, blockchain.transactionInfo(id).get._1))
  }

  def disallowBeforeActivationTime[T <: Transaction](blockchain: Blockchain, height: Int, tx: T): Either[ValidationError, T] = {

    def activationBarrier(b: BlockchainFeature, msg: Option[String] = None): Either[ActivationError, T] =
      Either.cond(
        blockchain.isFeatureActivated(b, height),
        tx,
        ValidationError.ActivationError(msg.getOrElse(b.description + " feature has not been activated yet"))
      )

    def scriptActivation(sc: Script): Either[ActivationError, T] = {

      val ab = activationBarrier(BlockchainFeatures.Ride4DApps)

      def scriptVersionActivation(sc: Script): Either[ActivationError, T] = sc.stdLibVersion match {
        case V1 | V2 if sc.containsBlockV2.value => ab
        case V1 | V2                             => Right(tx)
        case V3                                  => ab
      }

      def scriptTypeActivation(sc: Script): Either[ActivationError, T] = sc match {
        case e: ExprScript                        => Right(tx)
        case c: ContractScript.ContractScriptImpl => ab
      }

      for {
        _ <- scriptVersionActivation(sc)
        _ <- scriptTypeActivation(sc)
      } yield tx

    }

    tx match {
      case _: BurnTransactionV1     => Right(tx)
      case _: PaymentTransaction    => Right(tx)
      case _: GenesisTransaction    => Right(tx)
      case _: TransferTransactionV1 => Right(tx)
      case _: IssueTransactionV1    => Right(tx)
      case _: ReissueTransactionV1  => Right(tx)
      case _: ExchangeTransactionV1 => Right(tx)

      case exv2: ExchangeTransactionV2 =>
        activationBarrier(BlockchainFeatures.SmartAccountTrading).flatMap { tx =>
          (exv2.buyOrder, exv2.sellOrder) match {
            case (_: OrderV3, _: Order) | (_: Order, _: OrderV3) => activationBarrier(BlockchainFeatures.OrderV3)
            case _                                               => Right(tx)
          }
        }

      case _: LeaseTransactionV1       => Right(tx)
      case _: LeaseCancelTransactionV1 => Right(tx)
      case _: CreateAliasTransactionV1 => Right(tx)
      case _: MassTransferTransaction  => activationBarrier(BlockchainFeatures.MassTransfer)
      case _: DataTransaction          => activationBarrier(BlockchainFeatures.DataTransaction)

      case sst: SetScriptTransaction =>
        sst.script match {
          case None     => Right(tx)
          case Some(sc) => scriptActivation(sc)
        }

      case _: TransferTransactionV2 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case it: IssueTransactionV2 =>
        it.script match {
          case None     => Right(tx)
          case Some(sc) => scriptActivation(sc)
        }

      case it: SetAssetScriptTransaction =>
        it.script match {
          case None     => Left(GenericError("Cannot set empty script"))
          case Some(sc) => scriptActivation(sc)
        }

      case _: ReissueTransactionV2     => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: BurnTransactionV2        => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: LeaseTransactionV2       => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: LeaseCancelTransactionV2 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: CreateAliasTransactionV2 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: SponsorFeeTransaction    => activationBarrier(BlockchainFeatures.FeeSponsorship)
      case _: InvokeScriptTransaction  => activationBarrier(BlockchainFeatures.Ride4DApps)
      case _                           => Left(GenericError("Unknown transaction must be explicitly activated"))
    }
  }

  def disallowTxFromFuture[T <: Transaction](settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {
    val allowTransactionsFromFutureByTimestamp = tx.timestamp < settings.allowTransactionsFromFutureUntil
    if (!allowTransactionsFromFutureByTimestamp && tx.timestamp - time > settings.maxTransactionTimeForwardOffset.toMillis)
      Left(Mistiming(s"""Transaction timestamp ${tx.timestamp}
       |is more than ${settings.maxTransactionTimeForwardOffset.toMillis}ms in the future
       |relative to block timestamp $time""".stripMargin.replaceAll("\n", " ")))
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](settings: FunctionalitySettings, prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > settings.maxTransactionTimeBackOffset.toMillis =>
        Left(Mistiming(s"""Transaction timestamp ${tx.timestamp}
         |is more than ${settings.maxTransactionTimeBackOffset.toMillis}ms in the past
         |relative to previous block timestamp $prevBlockTime""".stripMargin.replaceAll("\n", " ")))
      case _ => Right(tx)
    }

  private def feeInUnits(blockchain: Blockchain, height: Int, tx: Transaction): Either[ValidationError, Long] = {
    FeeConstants
      .get(tx.builder.typeId)
      .map { baseFee =>
        tx match {
          case tx: MassTransferTransaction =>
            baseFee + (tx.transfers.size + 1) / 2
          case tx: DataTransaction =>
            val base = if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height)) tx.bodyBytes() else tx.bytes()
            baseFee + (base.length - 1) / 1024
          case _ => baseFee
        }
      }
      .toRight(UnsupportedTransactionType)
  }

  def getMinFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, (Asset, Long, Long)] = {
    type FeeInfo = (Option[(Asset, AssetDescription)], Long)

    def feeAfterSponsorship(txAsset: Asset): Either[ValidationError, FeeInfo] =
      if (height < Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) {
        // This could be true for private blockchains
        feeInUnits(blockchain, height, tx).map(x => (None, x * FeeUnit))
      } else
        for {
          feeInUnits <- feeInUnits(blockchain, height, tx)
          r <- txAsset match {
            case Waves => Right((None, feeInUnits * FeeUnit))
            case assetId @ IssuedAsset(_) =>
              for {
                assetInfo <- blockchain
                  .assetDescription(assetId)
                  .toRight(GenericError(s"Asset ${assetId.id.base58} does not exist, cannot be used to pay fees"))
                wavesFee <- Either.cond(
                  assetInfo.sponsorship > 0,
                  feeInUnits * FeeUnit,
                  GenericError(s"Asset ${assetId.id.base58} is not sponsored, cannot be used to pay fees")
                )
              } yield (Some((assetId, assetInfo)), wavesFee)
          }
        } yield r

    def isSmartToken(input: FeeInfo): Boolean =
      input._1
        .map(_._1)
        .flatMap {
          case a @ IssuedAsset(_) => blockchain.assetDescription(a)
          case Waves              => None
        }
        .exists(_.script.isDefined)

    def feeAfterSmartTokens(inputFee: FeeInfo): Either[ValidationError, FeeInfo] = {
      val (feeAssetInfo, feeAmount) = inputFee
      val assetsCount = tx match {
        case tx: ExchangeTransaction => tx.checkedAssets().collect { case a @ IssuedAsset(_) => a }.count(blockchain.hasAssetScript) /* *3 if we deside to check orders and transaction */
        case _                       => tx.checkedAssets().collect { case a @ IssuedAsset(_) => a }.count(blockchain.hasAssetScript)
      }
      if (isSmartToken(inputFee)) {
        //Left(GenericError("Using smart asset for sponsorship is disabled."))
        Right { (feeAssetInfo, feeAmount + ScriptExtraFee * (1 + assetsCount)) }
      } else {
        Right { (feeAssetInfo, feeAmount + ScriptExtraFee * assetsCount) }
      }
    }

    def smartAccountScriptsCount: Int = tx match {
      case tx: Transaction with Authorized => cond(blockchain.hasScript(tx.sender))(1, 0)
      case _                               => 0
    }

    def feeAfterSmartAccounts(inputFee: FeeInfo): Either[ValidationError, FeeInfo] = Right {
      val extraFee                  = smartAccountScriptsCount * ScriptExtraFee
      val (feeAssetInfo, feeAmount) = inputFee
      (feeAssetInfo, feeAmount + extraFee)
    }

    feeAfterSponsorship(tx.assetFee._1)
      .flatMap(feeAfterSmartTokens)
      .flatMap(feeAfterSmartAccounts)
      .map {
        case (Some((assetId, assetInfo)), amountInWaves) =>
          (assetId, Sponsorship.fromWaves(amountInWaves, assetInfo.sponsorship), amountInWaves)
        case (None, amountInWaves) => (Waves, amountInWaves, amountInWaves)
      }
  }

  def checkFee(blockchain: Blockchain, fs: FunctionalitySettings, height: Int, tx: Transaction): Either[ValidationError, Unit] = {
    if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) {
      for {
        minAFee <- getMinFee(blockchain, fs, height, tx)
        minWaves   = minAFee._3
        minFee     = minAFee._2
        feeAssetId = minAFee._1
        _ <- Either.cond(
          minFee <= tx.assetFee._2,
          (),
          GenericError(
            s"Fee in ${feeAssetId.fold("WAVES")(_.id.base58)} for ${tx.builder.classTag} does not exceed minimal value of $minWaves WAVES: ${tx.assetFee._2}")
        )
      } yield ()
    } else {
      Either.cond(tx.assetFee._2 > 0 || !tx.isInstanceOf[Authorized], (), GenericError(s"Fee must be positive."))
    }
  }

  def cond[A](c: Boolean)(a: A, b: A): A = if (c) a else b

  def validateOverflow(dataList: Traversable[Long], errMsg: String): Either[ValidationError, Unit] = {
    Try(dataList.foldLeft(0L)(Math.addExact))
      .fold(
        _ => GenericError(errMsg).asLeft[Unit],
        _ => ().asRight[ValidationError]
      )
  }
}
