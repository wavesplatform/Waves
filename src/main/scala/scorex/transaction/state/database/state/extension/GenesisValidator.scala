package scorex.transaction.state.database.state.extension

import scorex.transaction.{GenesisTransaction, Transaction}

class GenesisValidator extends StateExtension {


  override def isValid(tx: Transaction, height: Int): Boolean = tx match {
    case gtx: GenesisTransaction => height == 0
    case _ => true
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {}
}