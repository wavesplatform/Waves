package com.wavesplatform.state2

import java.util

import scorex.account.Account

package object diffs {
  def ensureSenderHasEnoughBalance(s: StateWriter)(sender: Account, assets: List[ByteArray]): Unit = {
    s.applyBlockDiff(Diff(Map.empty,
      Map(sender -> Portfolio(Long.MaxValue - 1, Long.MaxValue - 1, assets.map(a => a -> (Long.MaxValue - 1)).toMap)),
      assets.map(a => a -> AssetInfo(isReissuable = true, Long.MaxValue - 1)).toMap
    ).asBlockDiff)
  }


  class TestStorage extends JavaMapStorage {
    override val transactions = new util.HashMap[Array[Byte], (Int, Array[Byte])]
    override val portfolios = new util.HashMap[Array[Byte], (Long, Long, Map[Array[Byte], Long])]
    override val assets = new util.HashMap[Array[Byte], (Boolean, Long)]
    override val accountTransactionIds = new util.HashMap[Array[Byte], util.List[Array[Byte]]]
    override val effectiveBalanceSnapshots = new util.HashMap[(Array[Byte], Int), (Long, Long)]
    override val paymentTransactionHashes = new util.HashMap[Array[Byte], Array[Byte]]
    override val maxPaymentTransactionTimestampInPreviousBlocks = new util.HashMap[Array[Byte], Long]

    var height: Int = 0

    override def getHeight: Int = height

    override def setHeight(i: Int): Unit = {
      height = i
    }
  }

}
