package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{Diff, EqByteArray, StateReader}
import scorex.account.{Account, Alias}
import scorex.settings.ChainParameters
import scorex.transaction.ValidationError.{AliasNotExists, TransactionValidationError}
import scorex.transaction._
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils.ByteArray

import scala.util.{Left, Right}

object GenesisTransactionDiff {

  def apply(s: StateReader)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    //    if (s.height)
    ???
  }
}

object BasicDiff {

  def apply[T <: Transaction](state: StateReader, settings: ChainParameters, time: Long)(transaction: T): Either[ValidationError, T] = {

    def disallowDuplicateIds(t: T): Either[StateValidationError, T] = t match {
      case tx: PaymentTransaction if tx.timestamp < settings.requirePaymentUniqueId => Right(t)
      case tx: Transaction => if (state.transactionInfo(EqByteArray(tx.id)).isEmpty) Right(t)
      else Left(TransactionValidationError(tx, "(except for some cases of PaymentTransaction) cannot be duplicated"))
    }

    def disallowBeforeActivationTime(tx: T): Either[StateValidationError, T] = tx match {
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

    def incrementingTimestamp(tx: T): Either[StateValidationError, T] = {

      def isTimestampCorrect(ptx: PaymentTransaction): Boolean = {
        val maybePreviousPmtTx: Option[PaymentTransaction] = state.accountTransactionIds(ptx.sender).toStream
          .flatMap(id => state.transactionInfo(id))
          .filter { case (id, t) => t.isInstanceOf[PaymentTransaction] }
          .collectFirst { case (id, t) => t.asInstanceOf[PaymentTransaction] }

        maybePreviousPmtTx match {
          case Some(lastTransaction) => lastTransaction.timestamp < ptx.timestamp
          case None => true
        }
      }

      tx match {
        case ptx: PaymentTransaction =>
          val isCorrect = tx.timestamp < settings.allowInvalidPaymentTransactionsByTimestamp || isTimestampCorrect(ptx)
          if (isCorrect) Right(tx)
          else Left(TransactionValidationError(tx, s" is earlier than previous transaction after time=${settings.allowInvalidPaymentTransactionsByTimestamp}"))
        case _ => Right(tx)
      }
    }

    def addressAliasExists(tx: T): Either[StateValidationError, T] = {
      val maybeAlias = tx match {
        case ltx: LeaseTransaction => ltx.recipient match {
          case a: Account => None
          case a: Alias => Some(a)
        }
        case ttx: TransferTransaction => ttx.recipient match {
          case a: Account => None
          case a: Alias => Some(a)
        }
        case _ => None
      }

      maybeAlias match {
        case None => Right(tx)
        //        case Some(al) => state.resolveAlias(al) match {
        //          case Some(add) => Right(tx)
        //          case None => Left(AliasNotExists(al))
        //        }
      }
    }

    disallowDuplicateIds(transaction)
      .flatMap(disallowBeforeActivationTime)
      .flatMap(incrementingTimestamp)
      .flatMap(addressAliasExists)
  }
}
