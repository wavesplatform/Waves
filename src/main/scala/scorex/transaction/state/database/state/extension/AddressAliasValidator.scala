package scorex.transaction.state.database.state.extension

import scorex.account.{Account, Alias}
import scorex.transaction.{AliasTransaction, Transaction}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.storage.{AliasExtendedStorageI, StateStorageI}

class AddressAliasValidator(storage: StateStorageI with AliasExtendedStorageI) extends Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[TransactionValidationError, Transaction] = {

    val maybeAlias = tx match {
      case ltx: LeaseTransaction => ltx.recipient match {
        case a: Account => None
        case a: Alias => Some(a)
      }
      case ttx: TransferTransaction => ttx.recipient match {
        case a: Account => None
        case a: Alias => Some(a)
      }
      case _ => None
    }

    maybeAlias match {
      case None => Right(tx)
      case Some(al) => storage.resolveAlias(al.name) match {
        case Some(add) => Right(tx)
        case None => Left(TransactionValidationError(tx, "Alias doesn't exist"))
      }
    }
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = tx match {
    case at: AliasTransaction => storedState.persistAlias(at.sender, at.alias)
    case _ => ()
  }
}
