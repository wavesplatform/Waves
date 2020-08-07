package com.wavesplatform.events

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff

trait BlockchainUpdateTriggers {
  def onProcessBlock(block: Block, diff: DetailedDiff, minerReward: Option[Long], blockchainBeforeWithMinerReward: Blockchain): Unit
  def onProcessMicroBlock(
      microBlock: MicroBlock,
      diff: DetailedDiff,
      blockchainBeforeWithMinerReward: Blockchain,
      totalBlockId: ByteStr,
      totalTransactionsRoot: ByteStr
  ): Unit
  def onRollback(toBlockId: ByteStr, toHeight: Int): Unit
  def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit
}

object BlockchainUpdateTriggers {
  def noop: BlockchainUpdateTriggers = new BlockchainUpdateTriggers {
    override def onProcessBlock(block: Block, diff: DetailedDiff, minerReward: Option[Long], blockchainBeforeWithMinerReward: Blockchain): Unit = {}
    override def onProcessMicroBlock(
        microBlock: MicroBlock,
        diff: DetailedDiff,
        blockchainBeforeWithMinerReward: Blockchain,
        totalBlockId: ByteStr,
        totalTransactionsRoot: ByteStr
    ): Unit                                                                  = {}
    override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit         = {}
    override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = {}
  }
}
