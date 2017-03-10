package com.wavesplatform.state2

import scorex.transaction.Transaction


trait StateReader {
  def getTxInfo(id: Array[Byte]): Option[(Int, Transaction)]
}

trait StateWriter {
  def applyDiff(d: Diff): Unit
}