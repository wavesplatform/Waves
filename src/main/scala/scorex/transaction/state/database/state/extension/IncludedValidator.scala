package scorex.transaction.state.database.state.extension

import scorex.settings.ChainParameters
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.ValidationError.{CustomValidationError, StateValidationError}
import scorex.transaction.state.database.state.storage.StateStorageI
import scorex.transaction.{PaymentTransaction, Transaction, ValidationError}

class IncludedValidator(storage: StateStorageI, requirePaymentUniqueId: Long) extends Validator {


  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case tx: PaymentTransaction if tx.timestamp < requirePaymentUniqueId => Right(tx)
    case tx: Transaction => if (storage.included(tx.id, None).isEmpty) Right(tx)
    else Left(StateValidationError(s"Transaction(except for some cases of PaymentTransaction) cannot be duplicated"))
  }


  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = {
    storage.putTransaction(tx, height)
  }
}
