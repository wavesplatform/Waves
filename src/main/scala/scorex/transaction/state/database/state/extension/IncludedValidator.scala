package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.CustomValidationError
import scorex.transaction.state.database.state.storage.StateStorageI
import scorex.transaction.{PaymentTransaction, Transaction, ValidationError}

class IncludedValidator(storage: StateStorageI, requirePaymentUniqueId: Long) extends Validator {


  override def isValid(tx: Transaction, height: Int): Either[ValidationError, Transaction] = tx match {
    case tx: PaymentTransaction =>
      if (tx.timestamp < requirePaymentUniqueId) Right(tx)
      else Left(CustomValidationError(s"PaymentTransaction(time=${tx.timestamp}) cannot be duplicated after time=$requirePaymentUniqueId"))
    case tx: Transaction => if (storage.included(tx.id, None).isEmpty) Right(tx)
    else Left(CustomValidationError(s"Transaction(except for some cases of PaymentTransaction) cannot be duplicated"))
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {
    storage.putTransaction(tx, height)
  }
}
