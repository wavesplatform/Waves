package com.wavesplatform.events

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff

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

  def combined(triggers: => Seq[BlockchainUpdateTriggers]): BlockchainUpdateTriggers = new BlockchainUpdateTriggers {
    override def onProcessBlock(
        block: Block,
        diff: BlockDiffer.DetailedDiff,
        minerReward: Option[Long],
        blockchainBeforeWithMinerReward: Blockchain
    ): Unit =
      triggers.foreach(_.onProcessBlock(block, diff, minerReward, blockchainBeforeWithMinerReward))

    override def onProcessMicroBlock(
        microBlock: MicroBlock,
        diff: BlockDiffer.DetailedDiff,
        blockchainBeforeWithMinerReward: Blockchain,
        totalBlockId: ByteStr,
        totalTransactionsRoot: ByteStr
    ): Unit =
      triggers.foreach(_.onProcessMicroBlock(microBlock, diff, blockchainBeforeWithMinerReward, totalBlockId, totalTransactionsRoot))

    override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit =
      triggers.foreach(_.onRollback(toBlockId, toHeight))

    override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit =
      triggers.foreach(_.onMicroBlockRollback(toBlockId, height))
  }
}

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
