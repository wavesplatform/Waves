package com.wavesplatform.state2

import scorex.transaction.{Transaction, TransactionParser}

class StateReaderWriter(p: PrimitiveStorage) extends StateReader with StateWriter {

  override def getTxInfo(id: Array[Byte]): Option[(Int, Transaction)] = Option(p.txs.get(id)).map {
    case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
  }

  override def applyDiff(d: Diff): Unit = {
    d.txs.foreach { case (id, (h, tx)) => p.txs.put(id, (h, tx.bytes))
    }
  }
}





