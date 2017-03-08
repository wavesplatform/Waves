package scorex.transaction.state.database.state.extension

import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{StateValidationError, Transaction}

trait Validator {
  def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction]
}

trait Processor {
  def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = ()
}


