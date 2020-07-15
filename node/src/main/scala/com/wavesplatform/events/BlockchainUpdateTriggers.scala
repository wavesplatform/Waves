package com.wavesplatform.events

import cats.Monoid
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff

trait BlockchainUpdateTriggers {
  def onProcessBlock(block: Block, diff: DetailedDiff, minerReward: Option[Long], blockchainBefore: Blockchain): Unit
  def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain, totalBlockId: ByteStr): Unit
  def onRollback(toBlockId: ByteStr, toHeight: Int): Unit
  def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit
}

object BlockchainUpdateTriggers {
  def noop: BlockchainUpdateTriggers = new BlockchainUpdateTriggers {
    override def onProcessBlock(block: Block, diff: DetailedDiff, minerReward: Option[Long], blockchainBefore: Blockchain): Unit = {}
    override def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain, totalBlockId: ByteStr): Unit = {}
    override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = {}
    override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = {}
  }

  implicit val monoid: Monoid[BlockchainUpdateTriggers] = new Monoid[BlockchainUpdateTriggers] {
    override val empty: BlockchainUpdateTriggers = noop

    override def combine(left: BlockchainUpdateTriggers, right: BlockchainUpdateTriggers): BlockchainUpdateTriggers =
      new BlockchainUpdateTriggers {
        override def onProcessBlock(block: Block, diff: DetailedDiff, minerReward: Option[Long], blockchainBefore: Blockchain): Unit = {
          left.onProcessBlock(block, diff, minerReward, blockchainBefore)
          right.onProcessBlock(block, diff, minerReward, blockchainBefore)
        }

        override def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain, totalBlockId: ByteStr): Unit = {
          left.onProcessMicroBlock(microBlock, diff, blockchainBefore, totalBlockId)
          right.onProcessMicroBlock(microBlock, diff, blockchainBefore, totalBlockId)
        }

        override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = {
          left.onRollback(toBlockId, toHeight)
          right.onRollback(toBlockId, toHeight)
        }

        override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit = {
          left.onMicroBlockRollback(toBlockId, height)
          right.onMicroBlockRollback(toBlockId, height)
        }
      }
  }
}


