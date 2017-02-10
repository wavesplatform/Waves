package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.{Transaction, ValidationError}

trait Validator {
  def isValid(tx: Transaction, height: Int): Either[StateValidationError, Transaction]

  def process(tx: Transaction, blockTs: Long, height: Int): Unit

}
