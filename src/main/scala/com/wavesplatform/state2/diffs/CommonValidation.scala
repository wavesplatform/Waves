package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{EqByteArray, Portfolio, _}
import scorex.account.Account
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scala.concurrent.duration._
import scala.util.{Left, Right}

object CommonValidation {

  val MaxTimeTransactionOverBlockDiff: FiniteDuration = 90.minutes

  def disallowSendingGreaterThanBalance[T <: Transaction](s: StateReader, settings: FunctionalitySettings, blockTime: Long, tx: T): Either[ValidationError, T] =
    if (blockTime >= settings.allowTemporaryNegativeUntil)
      tx match {
        case ptx: PaymentTransaction if s.accountPortfolio(ptx.sender).balance < (ptx.amount + ptx.fee) =>
          Left(TransactionValidationError(ptx, s"Attempt to pay unavailable funds: balance " +
            s"${s.accountPortfolio(ptx.sender).balance} is less than ${ptx.amount + ptx.fee}"))
        case ttx: TransferTransaction =>
          val sender: Account = ttx.sender

          val amountDiff = ttx.assetId match {
            case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(EqByteArray(aid) -> -ttx.amount))
            case None => Portfolio(-ttx.amount, LeaseInfo.empty, Map.empty)
          }
          val feeDiff = ttx.feeAssetId match {
            case Some(aid) => Portfolio(0, LeaseInfo.empty, Map(EqByteArray(aid) -> -ttx.fee))
            case None => Portfolio(-ttx.fee, LeaseInfo.empty, Map.empty)
          }

          val accountPortfolio = s.accountPortfolio(sender)
          val spendings = Monoid.combine(amountDiff, feeDiff)
          accountPortfolio.balance + spendings.balance


          lazy val negativeAssets: Boolean = spendings.assets.exists { case (id, amt) => (accountPortfolio.assets.getOrElse(id, 0L) + amt) < 0L }
          lazy val negativeWaves = accountPortfolio.balance + spendings.balance < 0
          if (negativeWaves || negativeAssets)
            Left(TransactionValidationError(ttx, s"Attempt to transfer unavailable funds:" +
              s" Transaction application leads from $accountPortfolio to (at least) temporary negative state"))
          else Right(tx)
        case _ => Right(tx)
      } else Right(tx)

  def disallowDuplicateIds[T <: Transaction](state: StateReader, settings: FunctionalitySettings, height: Int, tx: T): Either[ValidationError, T] = tx match {
    case ptx: PaymentTransaction if ptx.timestamp < settings.requirePaymentUniqueId => Right(tx)
    case _ =>
      if (state.containsTransaction(EqByteArray(tx.id)))
        Left(TransactionValidationError(tx, s"Tx id(exc. for some PaymentTransactions) cannot be duplicated. Current height is: $height. Tx with such id aready present"))
      else Right(tx)
  }

  def disallowBeforeActivationTime[T <: Transaction](settings: FunctionalitySettings, tx: T): Either[ValidationError, T] =
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
      case tx: MakeAssetNameUniqueTransaction if tx.timestamp <= settings.allowMakeAssetNameUniqueTransactionAfterTimestamp =>
        Left(TransactionValidationError(tx, s"must not appear before time=${settings.allowMakeAssetNameUniqueTransactionAfterTimestamp}"))
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
      case _: MakeAssetNameUniqueTransaction => Right(tx)
      case x => Left(TransactionValidationError(x, "Unknown transaction must be explicitly registered within ActivatedValidator"))
    }

  def disallowTxFromFuture[T <: Transaction](settings: FunctionalitySettings, time: Long, tx: T): Either[ValidationError, T] = {

    val allowTransactionsFromFutureByTimestamp = tx.timestamp < settings.allowTransactionsFromFutureUntil
    if (allowTransactionsFromFutureByTimestamp) {
      Right(tx)
    } else {
      if ((tx.timestamp - time).millis <= MaxTimeTransactionOverBlockDiff)
        Right(tx)
      else Left(TransactionValidationError(tx, s"Transaction is from far future. BlockTime: $time"))
    }
  }
}


