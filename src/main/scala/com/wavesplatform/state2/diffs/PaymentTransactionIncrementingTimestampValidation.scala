package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.StateReader
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{PaymentTransaction, StateValidationError}

import scala.util.{Left, Right}

object PaymentTransactionIncrementingTimestampValidation {
  def apply[T](stateReader: StateReader, settings: FunctionalitySettings)(tx: T): Either[StateValidationError, T] = {

    def isTimestampCorrect(ptx: PaymentTransaction): Boolean = {
      val maybePreviousPmtTx: Option[PaymentTransaction] = stateReader.lastAccountPaymentTransaction(ptx.sender)
      maybePreviousPmtTx match {
        case Some(lastTransaction) => lastTransaction.timestamp < ptx.timestamp
        case None => true
      }
    }

    tx match {
      case ptx: PaymentTransaction =>
        val isCorrect = ptx.timestamp < settings.allowInvalidPaymentTransactionsByTimestamp || isTimestampCorrect(ptx)
        if (isCorrect) Right(tx)
        else Left(TransactionValidationError(ptx, s" is earlier than previous transaction after time=${settings.allowInvalidPaymentTransactionsByTimestamp}"))
      case _ => Right(tx)
    }
  }

}
