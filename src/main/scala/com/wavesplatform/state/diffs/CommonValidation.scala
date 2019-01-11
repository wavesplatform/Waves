package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.account.Address
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.ValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer._

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration     = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours
  val ScriptExtraFee                                      = 400000L
  val FeeUnit                                             = 100000

  val FeeConstants: Map[Byte, Long] = Map(
    GenesisTransaction.typeId        -> 0,
    PaymentTransaction.typeId        -> 1,
    IssueTransaction.typeId          -> 1000,
    ReissueTransaction.typeId        -> 1000,
    BurnTransaction.typeId           -> 1,
    TransferTransaction.typeId       -> 1,
    MassTransferTransaction.typeId   -> 1,
    LeaseTransaction.typeId          -> 1,
    LeaseCancelTransaction.typeId    -> 1,
    ExchangeTransaction.typeId       -> 3,
    CreateAliasTransaction.typeId    -> 1,
    DataTransaction.typeId           -> 1,
    SetScriptTransaction.typeId      -> 10,
    SponsorFeeTransaction.typeId     -> 1000,
    SetAssetScriptTransaction.typeId -> (1000 - 4)
  )

  def disallowSendingGreaterThanBalance[T <: Transaction](blockchain: Blockchain,
                                                          settings: FunctionalitySettings,
                                                          blockTime: Long,
                                                          tx: T): Either[ValidationError, T] =
    if (blockTime >= settings.allowTemporaryNegativeUntil) {
      def checkTransfer(sender: Address, assetId: Option[AssetId], amount: Long, feeAssetId: Option[AssetId], feeAmount: Long) = {
        val amountDiff = assetId match {
          case Some(aid) => Portfolio(0, LeaseBalance.empty, Map(aid -> -amount))
          case None      => Portfolio(-amount, LeaseBalance.empty, Map.empty)
        }
        val feeDiff = feeAssetId match {
          case Some(aid) => Portfolio(0, LeaseBalance.empty, Map(aid -> -feeAmount))
          case None      => Portfolio(-feeAmount, LeaseBalance.empty, Map.empty)
        }

        val spendings       = Monoid.combine(amountDiff, feeDiff)
        val oldWavesBalance = blockchain.portfolio(sender).balance

        val newWavesBalance = oldWavesBalance + spendings.balance
        if (newWavesBalance < 0) {
          Left(
            GenericError(
              "Attempt to transfer unavailable funds: Transaction application leads to " +
                s"negative waves balance to (at least) temporary negative state, current balance equals $oldWavesBalance, " +
                s"spends equals ${spendings.balance}, result is $newWavesBalance"))
        } else if (spendings.assets.nonEmpty) {
          val oldAssetBalances = blockchain.portfolio(sender).assets
          val balanceError = spendings.assets.collectFirst {
            case (aid, delta) if oldAssetBalances.getOrElse(aid, 0L) + delta < 0 =>
              val availableBalance = oldAssetBalances.getOrElse(aid, 0L)
              GenericError(
                "Attempt to transfer unavailable funds: Transaction application leads to negative asset " +
                  s"'$aid' balance to (at least) temporary negative state, current balance is $availableBalance, " +
                  s"spends equals $delta, result is ${availableBalance + delta}")
          }

          balanceError.fold[Either[ValidationError, T]](Right(tx))(Left(_))
        } else Right(tx)
      }

      tx match {
        case ptx: PaymentTransaction if blockchain.portfolio(ptx.sender).balance < (ptx.amount + ptx.fee) =>
          Left(
            GenericError(
              "Attempt to pay unavailable funds: balance " +
                s"${blockchain.portfolio(ptx.sender).balance} is less than ${ptx.amount + ptx.fee}"))
        case ttx: TransferTransaction     => checkTransfer(ttx.sender, ttx.assetId, ttx.amount, ttx.feeAssetId, ttx.fee)
        case mtx: MassTransferTransaction => checkTransfer(mtx.sender, mtx.assetId, mtx.transfers.map(_.amount).sum, None, mtx.fee)
        case _                            => Right(tx)
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

    def activationBarrier(b: BlockchainFeature, msg: Option[String] = None) =
      Either.cond(
        blockchain.isFeatureActivated(b, height),
        tx,
        ValidationError.ActivationError(msg.getOrElse(tx.getClass.getSimpleName) + " has not been activated yet")
      )

    tx match {
      case _: BurnTransactionV1        => Right(tx)
      case _: PaymentTransaction       => Right(tx)
      case _: GenesisTransaction       => Right(tx)
      case _: TransferTransactionV1    => Right(tx)
      case _: IssueTransactionV1       => Right(tx)
      case _: ReissueTransactionV1     => Right(tx)
      case _: ExchangeTransactionV1    => Right(tx)
      case _: ExchangeTransactionV2    => activationBarrier(BlockchainFeatures.SmartAccountTrading)
      case _: LeaseTransactionV1       => Right(tx)
      case _: LeaseCancelTransactionV1 => Right(tx)
      case _: CreateAliasTransactionV1 => Right(tx)
      case _: MassTransferTransaction  => activationBarrier(BlockchainFeatures.MassTransfer)
      case _: DataTransaction          => activationBarrier(BlockchainFeatures.DataTransaction)
      case sst: SetScriptTransaction =>
        sst.script.map(_.version.value) match {
          case Some(1) | None => activationBarrier(BlockchainFeatures.SmartAccounts)
          case Some(2)        => activationBarrier(BlockchainFeatures.SmartAccountTrading, Some("Script version 2"))
          case Some(v)        => Left(GenericError(s"Bad script version $v"))
        }
      case _: TransferTransactionV2 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case it: IssueTransactionV2   => activationBarrier(if (it.script.isEmpty) BlockchainFeatures.SmartAccounts else BlockchainFeatures.SmartAssets)
      case it: SetAssetScriptTransaction =>
        if (it.script.isEmpty) {
          Left(GenericError("Cannot set empty script"))
        } else {
          activationBarrier(BlockchainFeatures.SmartAssets)
        }
      case _: ReissueTransactionV2     => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: BurnTransactionV2        => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: LeaseTransactionV2       => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: LeaseCancelTransactionV2 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: CreateAliasTransactionV2 => activationBarrier(BlockchainFeatures.SmartAccounts)
      case _: SponsorFeeTransaction    => activationBarrier(BlockchainFeatures.FeeSponsorship)
      case _                           => Left(GenericError("Unknown transaction must be explicitly activated"))
    }
  }

  def disallowTxFromFuture[T <: Transaction](settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {
    val allowTransactionsFromFutureByTimestamp = tx.timestamp < settings.allowTransactionsFromFutureUntil
    if (!allowTransactionsFromFutureByTimestamp && tx.timestamp - time > MaxTimeTransactionOverBlockDiff.toMillis)
      Left(Mistiming(s"Transaction ts ${tx.timestamp} is from far future. BlockTime: $time"))
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > MaxTimePrevBlockOverTransactionDiff.toMillis =>
        Left(Mistiming(s"Transaction ts ${tx.timestamp} is too old. Previous block time: $prevBlockTime"))
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

  def getMinFee(blockchain: Blockchain,
                fs: FunctionalitySettings,
                height: Int,
                tx: Transaction): Either[ValidationError, (Option[AssetId], Long, Long)] = {
    type FeeInfo = (Option[(AssetId, AssetDescription)], Long)

    def feeAfterSponsorship(txAsset: Option[AssetId]): Either[ValidationError, FeeInfo] =
      if (height < Sponsorship.sponsoredFeesSwitchHeight(blockchain, fs)) {
        // This could be true for private blockchains
        feeInUnits(blockchain, height, tx).map(x => (None, x * FeeUnit))
      } else
        for {
          feeInUnits <- feeInUnits(blockchain, height, tx)
          r <- txAsset match {
            case None => Right((None, feeInUnits * FeeUnit))
            case Some(assetId) =>
              for {
                assetInfo <- blockchain.assetDescription(assetId).toRight(GenericError(s"Asset $assetId does not exist, cannot be used to pay fees"))
                wavesFee <- Either.cond(
                  assetInfo.sponsorship > 0,
                  feeInUnits * FeeUnit,
                  GenericError(s"Asset $assetId is not sponsored, cannot be used to pay fees")
                )
              } yield (Some((assetId, assetInfo)), wavesFee)
          }
        } yield r

    def isSmartToken(input: FeeInfo): Boolean = input._1.map(_._1).flatMap(blockchain.assetDescription).exists(_.script.isDefined)

    def feeAfterSmartTokens(inputFee: FeeInfo): Either[ValidationError, FeeInfo] = {
      val (feeAssetInfo, feeAmount) = inputFee
      val assetsCount = tx match {
        case tx: ExchangeTransaction => tx.checkedAssets().count(blockchain.hasAssetScript) /* *3 if we deside to check orders and transaction */
        case _                       => tx.checkedAssets().count(blockchain.hasAssetScript)
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
          (Some(assetId), Sponsorship.fromWaves(amountInWaves, assetInfo.sponsorship), amountInWaves)
        case (None, amountInWaves) => (None, amountInWaves, amountInWaves)
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
            s"Fee in ${feeAssetId.fold("WAVES")(_.toString)} for ${tx.builder.classTag} does not exceed minimal value of $minWaves WAVES: ${tx.assetFee._2}")
        )
      } yield ()
    } else {
      Either.cond(tx.assetFee._2 > 0 || !tx.isInstanceOf[Authorized], (), GenericError(s"Fee must be positive."))
    }
  }

  def cond[A](c: Boolean)(a: A, b: A): A = if (c) a else b
}
