package scorex.transaction.state.database.state.extension

import scorex.account.Alias
import scorex.transaction.{StateValidationError, Transaction}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.storage.{AliasExtendedStorageI, StateStorageI}

class LeaseToSelfAliasValidator(storage: StateStorageI with AliasExtendedStorageI) extends Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = {

    tx match {
      case ltx: LeaseTransaction =>
        ltx.recipient match {
          case a: Alias => storedState.resolveAlias(a) match {
            case Some(acc) if ltx.sender.address == acc.address => Left(TransactionValidationError(tx, "Cannot lease to own alias"))
            case _ => Right(tx)
          }
          case _ => Right(tx)
        }
      case _ => Right(tx)
    }
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = () // No alias tx just yet
}
