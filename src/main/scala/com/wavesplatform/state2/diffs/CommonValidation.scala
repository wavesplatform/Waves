package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{Portfolio, _}
import scorex.account.Address
import scorex.transaction.ValidationError.{AlreadyInTheState, GenericError, Mistiming}
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours

  def disallowSendingGreaterThanBalance[T <: Transaction](s: SnapshotStateReader, settings: FunctionalitySettings, blockTime: Long, tx: T): Either[ValidationError, T] =
    if (blockTime >= settings.allowTemporaryNegativeUntil) {
      def checkTransfer(sender: Address, assetId: Option[AssetId], amount: Long, feeAssetId: Option[AssetId], feeAmount: Long) = {
        val amountDiff = assetId match {
          case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(aid -> -amount))
          case None => Portfolio(-amount, LeaseInfo.empty, Map.empty)
        }
        val feeDiff = feeAssetId match {
          case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(aid -> -feeAmount))
          case None => Portfolio(-feeAmount, LeaseInfo.empty, Map.empty)
        }

        val spendings = Monoid.combine(amountDiff, feeDiff)
        val accountPortfolio = s.partialPortfolio(sender, spendings.assets.keySet)

        lazy val negativeAsset = spendings.assets.find { case (id, amt) => (accountPortfolio.assets.getOrElse(id, 0L) + amt) < 0L }.map { case (id, amt) => (id, accountPortfolio.assets.getOrElse(id, 0L), amt, accountPortfolio.assets.getOrElse(id, 0L) + amt) }
        lazy val newWavesBalance = accountPortfolio.balance + spendings.balance
        lazy val negativeWaves = newWavesBalance < 0
        if (negativeWaves)
          Left(GenericError(s"Attempt to transfer unavailable funds:" +
            s" Transaction application leads to negative waves balance to (at least) temporary negative state, current balance equals ${accountPortfolio.balance}, spends equals ${spendings.balance}, result is $newWavesBalance"))
        else if (negativeAsset.nonEmpty)
          Left(GenericError(s"Attempt to transfer unavailable funds:" +
            s" Transaction application leads to negative asset '${negativeAsset.get._1}' balance to (at least) temporary negative state, current balance is ${negativeAsset.get._2}, spends equals ${negativeAsset.get._3}, result is ${negativeAsset.get._4}"))
        else Right(tx)
      }

      tx match {
        case ptx: PaymentTransaction if s.partialPortfolio(ptx.sender).balance < (ptx.amount + ptx.fee) =>
          Left(GenericError(s"Attempt to pay unavailable funds: balance " +
            s"${s.partialPortfolio(ptx.sender).balance} is less than ${ptx.amount + ptx.fee}"))
        case ttx: TransferTransaction => checkTransfer(ttx.sender, ttx.assetId, ttx.amount, ttx.feeAssetId, ttx.fee)
        case mtx: MassTransferTransaction => checkTransfer(mtx.sender, mtx.assetId, mtx.transfers.map(_._2).sum, None, mtx.fee)
        case _ => Right(tx)
      }
    } else Right(tx)

  def disallowDuplicateIds[T <: Transaction](state: SnapshotStateReader, settings: FunctionalitySettings, height: Int, tx: T): Either[ValidationError, T] = tx match {
    case ptx: PaymentTransaction if ptx.timestamp < settings.requirePaymentUniqueIdAfter => Right(tx)
    case _ =>
      state.transactionInfo(tx.id()) match {
        case Some((txHeight, _)) => Left(AlreadyInTheState(tx.id(), txHeight))
        case None => Right(tx)
      }
  }

  def disallowBeforeActivationTime[T <: Transaction](featureProvider: FeatureProvider, height: Int, tx: T): Either[ValidationError, T] =
    tx match {
      case _: BurnTransaction => Right(tx)
      case _: PaymentTransaction => Right(tx)
      case _: GenesisTransaction => Right(tx)
      case _: TransferTransaction => Right(tx)
      case _: IssueTransaction => Right(tx)
      case _: ReissueTransaction => Right(tx)
      case _: ExchangeTransaction => Right(tx)
      case _: LeaseTransaction => Right(tx)
      case _: LeaseCancelTransaction => Right(tx)
      case _: CreateAliasTransaction => Right(tx)
      case _: MassTransferTransaction =>
        Either.cond(featureProvider.isFeatureActivated(BlockchainFeatures.MassTransfer, height),
          tx, ValidationError.ActivationError("MassTransfer transaction has not been activated yet"))
      case _ => Left(GenericError("Unknown transaction must be explicitly activated"))
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
}


