package scorex.transaction.state.database.state.extension

import scorex.settings.ChainParameters
import scorex.transaction.state.database.state.storage.StateStorageI
import scorex.transaction.{PaymentTransaction, Transaction}

class IncludedValidator(storage: StateStorageI, settings: ChainParameters) extends StateExtension {


  override def isValid(tx: Transaction, height: Int): Boolean = tx match {
    case tx: PaymentTransaction if tx.timestamp < settings.requirePaymentUniqueId => true
    case tx: Transaction => storage.included(tx.id, None).isEmpty
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {
    storage.putTransaction(tx, height)
  }
}
