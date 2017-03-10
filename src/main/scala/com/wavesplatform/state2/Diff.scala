package com.wavesplatform.state2


import scorex.transaction.Transaction

case class Diff(txs: Map[Array[Byte], (Int, Transaction)])

object Diff {
  def combine(older: Diff, newer: Diff): Diff = Diff(newer.txs ++ older.txs)
}