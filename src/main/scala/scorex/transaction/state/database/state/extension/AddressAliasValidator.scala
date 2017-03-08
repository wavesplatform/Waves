package scorex.transaction.state.database.state.extension

import scorex.account.{Account, Alias}
import scorex.transaction.{CreateAliasTransaction, StateValidationError, Transaction}
import scorex.transaction.ValidationError.{AliasNotExists, TransactionValidationError}
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.storage.{AliasExtendedStorageI, StateStorageI}

class AddressAliasValidator(storage: StateStorageI with AliasExtendedStorageI) extends Validator with Processor {



}
