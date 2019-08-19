package com.wavesplatform.state.diffs

import cats._
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{Order, _}
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._

import scala.util.{Left, Right, Try}

object CommonValidation {

  def disallowSendingGreaterThanBalance[T <: Transaction](blockchain: Blockchain,
                                                          blockTime: Long,
                                                          tx: T): Either[ValidationError, T] =
    if (blockTime >= blockchain.settings.functionalitySettings.allowTemporaryNegativeUntil) {
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
        TxValidationError.ActivationError(msg.getOrElse(b.description + " feature has not been activated yet"))
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

      case sast: SetAssetScriptTransaction =>
        activationBarrier(BlockchainFeatures.SmartAssets).flatMap { _ =>
          sast.script match {
            case None     => Left(GenericError("Cannot set empty script"))
            case Some(sc) => scriptActivation(sc)
          }
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
      Left(
        Mistiming(
          s"""Transaction timestamp ${tx.timestamp}
       |is more than ${settings.maxTransactionTimeForwardOffset.toMillis}ms in the future
       |relative to block timestamp $time""".stripMargin
            .replaceAll("\n", " ")
            .replaceAll("\r", "")))
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
              .replaceAll("\r", "")))
      case _ => Right(tx)
    }

  def validateOverflow(dataList: Traversable[Long], errMsg: String): Either[ValidationError, Unit] = {
    Try(dataList.foldLeft(0L)(Math.addExact))
      .fold(
        _ => GenericError(errMsg).asLeft[Unit],
        _ => ().asRight[ValidationError]
      )
  }
}
