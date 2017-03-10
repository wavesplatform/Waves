package com.wavesplatform.state2

import scorex.transaction.Transaction

/**
  * Created by ilyas on 10-Mar-17.
  */
class CompositeStateReader(s: StateReader, d: Diff) extends StateReader {
  override def getTxInfo(id: Array[Byte]): Option[(Int, Transaction)] = d.txs.get(id).orElse(s.getTxInfo(id))
}
