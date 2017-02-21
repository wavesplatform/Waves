package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{Transaction, ValidationError}

trait Validator {
  def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction]

  def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit

}
