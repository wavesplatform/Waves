package scorex.transaction.state.database.state.extension

import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{GenesisTransaction, Transaction}

class GenesisValidator extends StateExtension {


  override def isValid(storedState: StoredState, tx: Transaction, height: Int): Boolean = tx match {
    case gtx: GenesisTransaction => height == 0
    case _ => true
  }


  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = {}
}
