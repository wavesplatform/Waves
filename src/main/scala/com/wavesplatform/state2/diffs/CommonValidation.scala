package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.features.Functionalities
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Portfolio, _}
import scorex.account.Address
import scorex.transaction.ValidationError.{GenericError, Mistiming}
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes
  val MaxTimePrevBlockOverTransactionDiff: FiniteDuration = 2.hours

  def disallowSendingGreaterThanBalance[T <: Transaction](s: StateReader, fn: Functionalities, blockTime: Long, tx: T): Either[ValidationError, T] =
    fn.allowTemporaryNegativeUntil.check(blockTime).fold(_ =>
      tx match {
        case ptx: PaymentTransaction if s.accountPortfolio(ptx.sender).balance < (ptx.amount + ptx.fee) =>
          Left(GenericError(s"Attempt to pay unavailable funds: balance " +
            s"${s.accountPortfolio(ptx.sender).balance} is less than ${ptx.amount + ptx.fee}"))
        case ttx: TransferTransaction =>
          val sender: Address = ttx.sender

          val amountDiff = ttx.assetId match {
            case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(aid -> -ttx.amount))
            case None => Portfolio(-ttx.amount, LeaseInfo.empty, Map.empty)
          }
          val feeDiff = ttx.feeAssetId match {
            case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(aid -> -ttx.fee))
            case None => Portfolio(-ttx.fee, LeaseInfo.empty, Map.empty)
          }

          val accountPortfolio = s.accountPortfolio(sender)
          val spendings = Monoid.combine(amountDiff, feeDiff)

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
        case _ => Right(tx)
      }, _ => Right(tx))

  def disallowDuplicateIds[T <: Transaction](state: StateReader, fn: Functionalities, height: Int, tx: T): Either[ValidationError, T] = tx match {
    case ptx: PaymentTransaction if fn.requirePaymentUniqueIdAfter.check(ptx.timestamp).isLeft => Right(tx)
    case _ =>
      if (state.containsTransaction(tx.id))
        Left(GenericError(s"Tx id(exc. for some PaymentTransactions) cannot be duplicated. Current height is: $height. Tx with such id already present"))
      else Right(tx)
  }

  def disallowBeforeActivationTime[T <: Transaction](fn: Functionalities, tx: T): Either[ValidationError, T] =
    tx match {
      case _: BurnTransaction => fn.allowBurnTransactionAfter.check(tx.timestamp).fold(e => Left(GenericError(e.message)), _ => Right(tx))
      case _: LeaseTransaction => fn.allowLeaseTransactionAfter.check(tx.timestamp).fold(e => Left(GenericError(e.message)), _ => Right(tx))
      case _: LeaseCancelTransaction => fn.allowLeaseTransactionAfter.check(tx.timestamp).fold(e => Left(GenericError(e.message)), _ => Right(tx))
      case _: ExchangeTransaction => fn.allowExchangeTransactionAfter.check(tx.timestamp).fold(e => Left(GenericError(e.message)), _ => Right(tx))
      case _: CreateAliasTransaction => fn.allowCreateAliasTransactionAfter.check(tx.timestamp).fold(e => Left(GenericError(e.message)), _ => Right(tx))
      case _: PaymentTransaction => Right(tx)
      case _: GenesisTransaction => Right(tx)
      case _: TransferTransaction => Right(tx)
      case _: IssueTransaction => Right(tx)
      case _: ReissueTransaction => Right(tx)
      case _ => Left(GenericError("Unknown transaction must be explicitly registered within ActivatedValidator"))
    }

  def disallowTxFromFuture[T <: Transaction](fn: Functionalities, time: Long, tx: T): Either[ValidationError, T] = {
    if (fn.allowTransactionsFromFutureUntil.check(tx.timestamp).isLeft && (tx.timestamp - time).millis > MaxTimeTransactionOverBlockDiff)
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


