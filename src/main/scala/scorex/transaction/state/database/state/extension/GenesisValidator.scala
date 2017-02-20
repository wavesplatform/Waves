package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.{GenesisTransaction, Transaction, ValidationError}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{GenesisTransaction, Transaction}

class GenesisValidator extends Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError,Transaction] = tx match {
    case gtx: GenesisTransaction if height != 0 => Left(StateValidationError("GenesisTranaction cannot appear in non-initial block"))
    case _ => Right(tx)
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = {}
}
