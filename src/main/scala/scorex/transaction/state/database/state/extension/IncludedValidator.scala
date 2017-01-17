package scorex.transaction.state.database.state.extension

import scorex.settings.WavesHardForkParameters
import scorex.transaction.state.database.state.storage.StateStorageI
import scorex.transaction.{PaymentTransaction, Transaction}

class IncludedValidator(storage: StateStorageI, settings: WavesHardForkParameters) extends StateExtension {


  override def isValid(tx: Transaction): Boolean = tx match {
    case tx: PaymentTransaction if tx.timestamp < settings.requirePaymentNotIncludedAfterTimestamp => true
    case tx: Transaction => storage.included(tx.id, None).isEmpty
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {
    storage.putTransaction(tx, height)
  }
}
