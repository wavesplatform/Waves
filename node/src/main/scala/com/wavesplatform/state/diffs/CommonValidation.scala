package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.features.OverdraftValidationProvider._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures, RideVersionProvider}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeExpressionTransaction, InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._

import scala.util.{Left, Right}

object CommonValidation {

  def disallowSendingGreaterThanBalance[T <: Transaction](blockchain: Blockchain, blockTime: Long, tx: T): Either[ValidationError, T] =
    if (blockTime >= blockchain.settings.functionalitySettings.allowTemporaryNegativeUntil) {
      def checkTransfer(
          sender: Address,
          assetId: Asset,
          amount: Long,
          feeAssetId: Asset,
          feeAmount: Long,
          allowFeeOverdraft: Boolean = false
      ) = {
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

        val newWavesBalance     = oldWavesBalance + spendings.balance
        val feeUncheckedBalance = oldWavesBalance + amountDiff.balance

        val overdraftFilter = allowFeeOverdraft && feeUncheckedBalance >= 0
        if (!overdraftFilter && newWavesBalance < 0) {
          Left(
            GenericError(
              "Attempt to transfer unavailable funds: Transaction application leads to " +
                s"negative waves balance to (at least) temporary negative state, current balance equals $oldWavesBalance, " +
                s"spends equals ${spendings.balance}, result is $newWavesBalance"
            )
          )
        } else {
          val balanceError = spendings.assets.collectFirst {
            case (aid, delta) if delta < 0 && blockchain.balance(sender, aid) + delta < 0 =>
              val availableBalance = blockchain.balance(sender, aid)
              GenericError(
                "Attempt to transfer unavailable funds: Transaction application leads to negative asset " +
                  s"'$aid' balance to (at least) temporary negative state, current balance is $availableBalance, " +
                  s"spends equals $delta, result is ${availableBalance + delta}"
              )
          }
          balanceError.fold[Either[ValidationError, T]](Right(tx))(Left(_))
        }
      }

      tx match {
        case ptx: PaymentTransaction if blockchain.balance(ptx.sender.toAddress, Waves) < (ptx.amount + ptx.fee) =>
          Left(
            GenericError(
              "Attempt to pay unavailable funds: balance " +
                s"${blockchain.balance(ptx.sender.toAddress, Waves)} is less than ${ptx.amount + ptx.fee}"
            )
          )
        case ttx: TransferTransaction     => checkTransfer(ttx.sender.toAddress, ttx.assetId, ttx.amount, ttx.feeAssetId, ttx.fee)
        case mtx: MassTransferTransaction => checkTransfer(mtx.sender.toAddress, mtx.assetId, mtx.transfers.map(_.amount).sum, Waves, mtx.fee)
        case citx: InvokeScriptTransaction =>
          val foldPayments: Iterable[Payment] => Iterable[Payment] =
            if (blockchain.useCorrectPaymentCheck)
              _.groupBy(_.assetId)
                .map { case (assetId, p) => Payment(p.map(_.amount).sum, assetId) } else
              identity

          for {
            address <- blockchain.resolveAlias(citx.dApp)
            allowFeeOverdraft = blockchain.accountScript(address) match {
              case Some(AccountScriptInfo(_, ContractScriptImpl(version, _), _, _)) if version >= V4 && blockchain.useCorrectPaymentCheck => true
              case _                                                                                                                      => false
            }
            check <- foldPayments(citx.payments)
              .map(p => checkTransfer(citx.senderAddress, p.assetId, p.amount, citx.feeAssetId, citx.fee, allowFeeOverdraft))
              .find(_.isLeft)
              .getOrElse(Right(tx))
          } yield check

        case _ => Right(tx)
      }
    } else Right(tx)

  def disallowDuplicateIds[T <: Transaction](blockchain: Blockchain, tx: T): Either[ValidationError, T] = tx match {
    case _: PaymentTransaction => Right(tx)
    case _ =>
      val id = tx.id()
      Either.cond(!blockchain.containsTransaction(tx), tx, AlreadyInTheState(id, blockchain.transactionMeta(id).get.height))
  }

  def disallowFromAnotherNetwork[T <: Transaction](tx: T, currentChainId: Byte): Either[ValidationError, T] =
    Either.cond(
      tx.chainId == currentChainId,
      tx,
      GenericError(
        s"Address belongs to another network: expected: ${AddressScheme.current.chainId}(${AddressScheme.current.chainId.toChar}), actual: ${tx.chainId}(${tx.chainId.toChar})"
      )
    )

  def disallowBeforeActivationTime[T <: Transaction](blockchain: Blockchain, tx: T): Either[ValidationError, T] = {
    def activationBarrier(b: BlockchainFeature, msg: Option[String] = None): Either[ActivationError, T] =
      Either.cond(
        blockchain.isFeatureActivated(b, blockchain.height),
        tx,
        TxValidationError.ActivationError(msg.getOrElse(b.description + " feature has not been activated yet"))
      )

    def scriptActivation(sc: Script): Either[ActivationError, T] = {
      val barrierByVersion =
        RideVersionProvider.actualVersionByFeature.map { case (feature, version) => (version, activationBarrier(feature)) }.toMap

      def scriptVersionActivation(sc: Script): Either[ActivationError, T] = sc.stdLibVersion match {
        case V1 | V2 | V3 if sc.containsArray => barrierByVersion(V4)
        case V1 | V2 if sc.containsBlockV2()  => barrierByVersion(V3)
        case V1 | V2                          => Right(tx)
        case v                                => barrierByVersion(v)
      }

      def scriptTypeActivation(sc: Script): Either[ActivationError, T] = (sc: @unchecked) match {
        case _: ExprScript                        => Right(tx)
        case _: ContractScript.ContractScriptImpl => barrierByVersion(V3)
      }

      for {
        _ <- scriptVersionActivation(sc)
        _ <- scriptTypeActivation(sc)
      } yield tx

    }

    def generic1or2Barrier(t: VersionedTransaction): Either[ActivationError, T] = {
      if (t.version == 1.toByte) Right(tx)
      else if (t.version == 2.toByte) activationBarrier(BlockchainFeatures.SmartAccounts)
      else Right(tx)
    }

    val versionsBarrier = tx match {
      case p: PBSince if p.isProtobufVersion =>
        activationBarrier(BlockchainFeatures.BlockV5)

      case v: VersionedTransaction if !TransactionParsers.all.contains((v.tpe.id.toByte, v.version)) =>
        Left(UnsupportedTypeAndVersion(v.tpe.id.toByte, v.version))

      case _ =>
        Right(tx)
    }

    val typedBarrier = tx match {
      case _: PaymentTransaction => Right(tx)
      case _: GenesisTransaction => Right(tx)

      case e: ExchangeTransaction if e.version == TxVersion.V1 => Right(tx)
      case exv2: ExchangeTransaction if exv2.version >= TxVersion.V2 =>
        activationBarrier(BlockchainFeatures.SmartAccountTrading).flatMap { tx =>
          (exv2.buyOrder, exv2.sellOrder) match {
            case (o1, o2) if o1.version >= 3 || o2.version >= 3 => activationBarrier(BlockchainFeatures.OrderV3)
            case _                                              => Right(tx)
          }
        }

      case _: MassTransferTransaction => activationBarrier(BlockchainFeatures.MassTransfer)
      case _: DataTransaction         => activationBarrier(BlockchainFeatures.DataTransaction)

      case sst: SetScriptTransaction =>
        sst.script match {
          case None     => Right(tx)
          case Some(sc) => scriptActivation(sc)
        }

      case it: IssueTransaction =>
        it.script match {
          case None     => Right(tx)
          case Some(sc) => scriptActivation(sc)
        }

      case sast: SetAssetScriptTransaction =>
        activationBarrier(BlockchainFeatures.SmartAssets).flatMap { _ =>
          sast.script match {
            case None     => Right(tx)
            case Some(sc) => scriptActivation(sc)
          }
        }

      case t: TransferTransaction    => generic1or2Barrier(t)
      case t: CreateAliasTransaction => generic1or2Barrier(t)
      case t: LeaseTransaction       => generic1or2Barrier(t)
      case t: LeaseCancelTransaction => generic1or2Barrier(t)
      case t: ReissueTransaction     => generic1or2Barrier(t)
      case t: BurnTransaction        => generic1or2Barrier(t)

      case _: SponsorFeeTransaction   => activationBarrier(BlockchainFeatures.FeeSponsorship)
      case _: InvokeScriptTransaction => activationBarrier(BlockchainFeatures.Ride4DApps)

      case _: UpdateAssetInfoTransaction => activationBarrier(BlockchainFeatures.BlockV5)
      case iet: InvokeExpressionTransaction =>
        if (iet.version == 1) activationBarrier(BlockchainFeatures.RideV6)
        else Left(TxValidationError.ActivationError(s"Transaction version ${iet.version} has not been activated yet"))

      case _: EthereumTransaction => activationBarrier(BlockchainFeatures.RideV6)

      case _ => Left(GenericError("Unknown transaction must be explicitly activated"))
    }

    val proofsValidate = tx match {
      case s: ProvenTransaction =>
        Proofs
          .create(s.proofs.proofs)
          .map(_ => tx)

      case _ =>
        Right(tx)
    }

    for {
      _ <- versionsBarrier
      _ <- typedBarrier
      _ <- proofsValidate
    } yield tx
  }

  def disallowTxFromFuture[T <: Transaction](settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {
    val allowTransactionsFromFutureByTimestamp = tx.timestamp < settings.allowTransactionsFromFutureUntil
    if (!allowTransactionsFromFutureByTimestamp && tx.timestamp - time > settings.maxTransactionTimeForwardOffset.toMillis)
      Left(
        Mistiming(
          s"""Transaction timestamp ${tx.timestamp}
       |is more than ${settings.maxTransactionTimeForwardOffset.toMillis}ms in the future
       |relative to block timestamp $time""".stripMargin
            .replaceAll("\n", " ")
            .replaceAll("\r", "")
        )
      )
    else Right(tx)
  }

  def disallowTxFromPast[T <: Transaction](settings: FunctionalitySettings, prevBlockTime: Option[Long], tx: T): Either[ValidationError, T] =
    prevBlockTime match {
      case Some(t) if (t - tx.timestamp) > settings.maxTransactionTimeBackOffset.toMillis =>
        Left(
          Mistiming(
            s"""Transaction timestamp ${tx.timestamp}
         |is more than ${settings.maxTransactionTimeBackOffset.toMillis}ms in the past
         |relative to previous block timestamp $prevBlockTime""".stripMargin
              .replaceAll("\n", " ")
              .replaceAll("\r", "")
          )
        )
      case _ => Right(tx)
    }
}
