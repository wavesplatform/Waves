package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.storage.StateStorageI
import scorex.transaction.{PaymentTransaction, StateValidationError, Transaction}

class IncludedValidator(storage: StateStorageI, requirePaymentUniqueId: Long) extends Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case tx: PaymentTransaction if tx.timestamp < requirePaymentUniqueId => Right(tx)
    case tx: Transaction => if (storage.included(tx.id, None).isEmpty) Right(tx)
    else Left(TransactionValidationError(tx, "(except for some cases of PaymentTransaction) cannot be duplicated"))
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = {
    storage.putTransaction(tx, height)
  }

}
