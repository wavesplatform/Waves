package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.{EqByteArray, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Account, Alias}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.state2._

import cats._
import cats.implicits._
import cats.syntax.all._


import scala.util.{Left, Right}
import scala.concurrent.duration._

object CommonValidation {
  def disallowSendingGreaterThanBalance[T <: Transaction](s: StateReader, settings: FunctionalitySettings, blockTime: Long, tx: T): Either[ValidationError, T] =
    if (blockTime >= settings.allowTemporaryNegativeUntil)
      tx match {
        case ptx: PaymentTransaction if s.accountPortfolio(ptx.sender).balance < (ptx.amount + ptx.fee) =>
          Left(TransactionValidationError(ptx, "Attempt to transfer unavailable funds"))
        case ttx: TransferTransaction =>
          val sender: Account = ttx.sender

          val amountDiff = ttx.assetId match {
            case Some(aid) => Portfolio(0, 0, Map(EqByteArray(aid) -> -ttx.amount))
            case None => Portfolio(-ttx.amount, -ttx.amount, Map.empty)
          }
          val feeDiff = ttx.feeAssetId match {
            case Some(aid) => Portfolio(0, 0, Map(EqByteArray(aid) -> -ttx.fee))
            case None => Portfolio(-ttx.fee, -ttx.fee, Map.empty)
          }

          val temporaryStateWhileTransfer = Monoid[Portfolio].combineAll(Seq(s.accountPortfolio(sender), amountDiff, feeDiff))

          if (temporaryStateWhileTransfer.balance < 0 || temporaryStateWhileTransfer.assets.values.exists(_ < 0))
            Left(TransactionValidationError(ttx, s"Attempt to transfer unavailable funds:" +
              s" TransactionApplication leads from ${s.accountPortfolio(sender)} to temporary negative state: $temporaryStateWhileTransfer"))
          else Right(tx)
        case _ => Right(tx)
      } else Right(tx)

  def disallowDuplicateIds[T <: Transaction](state: StateReader, settings: FunctionalitySettings, height: Int, tx: T): Either[ValidationError, T] = tx match {
    case ptx: PaymentTransaction if ptx.timestamp < settings.requirePaymentUniqueId => Right(tx)
    case _ =>
      state.transactionInfo(EqByteArray(tx.id)) match {
        case None => Right(tx)
        case Some((oldH, oldTx)) => Left(TransactionValidationError(tx, s"Tx id(exc. for some PaymentTransactions) cannot be duplicated." +
          s" Current height is: $height. Tx with such id aready present at H=$oldH: $oldTx"))
      }
  }

  def disallowBeforeActivationTime[T <: Transaction](state: StateReader, settings: FunctionalitySettings, tx: T): Either[ValidationError, T] =
    tx match {
      case tx: BurnTransaction if tx.timestamp <= settings.allowBurnTransactionAfterTimestamp =>
        Left(TransactionValidationError(tx, s"must not appear before time=${settings.allowBurnTransactionAfterTimestamp}"))
      case tx: LeaseTransaction if tx.timestamp <= settings.allowLeaseTransactionAfterTimestamp =>
        Left(TransactionValidationError(tx, s"must not appear before time=${settings.allowLeaseTransactionAfterTimestamp}"))
      case tx: LeaseCancelTransaction if tx.timestamp <= settings.allowLeaseTransactionAfterTimestamp =>
        Left(TransactionValidationError(tx, s"must not appear before time=${settings.allowLeaseTransactionAfterTimestamp}"))
      case tx: ExchangeTransaction if tx.timestamp <= settings.allowExchangeTransactionAfterTimestamp =>
        Left(TransactionValidationError(tx, s"must not appear before time=${settings.allowExchangeTransactionAfterTimestamp}"))
      case tx: CreateAliasTransaction if tx.timestamp <= settings.allowCreateAliasTransactionAfterTimestamp =>
        Left(TransactionValidationError(tx, s"must not appear before time=${settings.allowCreateAliasTransactionAfterTimestamp}"))
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
      case x => Left(TransactionValidationError(x, "Unknown transaction must be explicitly registered within ActivatedValidator"))
    }

  def disallowTxFromFuture[T <: Transaction](state: StateReader, settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {
    val allowTransactionsFromFutureByTimestamp = tx.timestamp < settings.allowTransactionsFromFutureUntil
    if (allowTransactionsFromFutureByTimestamp) {
      Right(tx)
    } else {
      if ((tx.timestamp - time).millis <= SimpleTransactionModule.MaxTimeTransactionOverBlockDiff)
        Right(tx)
      else Left(TransactionValidationError(tx, s"Transaction is from far future. BlockTime: $time"))
    }
  }
}


