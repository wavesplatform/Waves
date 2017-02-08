package scorex.transaction.state.database.state.extension

import scorex.transaction.{Transaction, ValidationError}

trait Validator {
  def isValid(tx: Transaction, height: Int): Either[ValidationError, Transaction]

  def process(tx: Transaction, blockTs: Long, height: Int): Unit

}
