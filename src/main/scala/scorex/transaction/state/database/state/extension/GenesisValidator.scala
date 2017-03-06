package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{GenesisTransaction, StateValidationError, Transaction, ValidationError}
import scorex.transaction.state.database.blockchain.StoredState

class GenesisValidator extends Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError,Transaction] = tx match {
    case gtx: GenesisTransaction if height != 0 => Left(TransactionValidationError(tx, "GenesisTranaction cannot appear in non-initial block"))
    case _ => Right(tx)
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = {}
}
