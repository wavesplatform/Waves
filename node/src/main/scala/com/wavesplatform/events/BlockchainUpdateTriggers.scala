package com.wavesplatform.events

import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff

trait BlockchainUpdateTriggers {
  def onProcessBlock(block: Block, diff: DetailedDiff, blockchainBefore: Blockchain): Unit

  def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain): Unit

  def onRollback(toBlockId: ByteStr, toHeight: Int)

  def onMicroBlockRollback(toTotalResBlockSig: ByteStr): Unit
}

object BlockchainUpdateTriggers {
  def noop: BlockchainUpdateTriggers = new BlockchainUpdateTriggers {
    override def onProcessBlock(block: Block, diff: DetailedDiff, blockchainBefore: Blockchain): Unit = {}

    override def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain): Unit = {}

    override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = {}

    override def onMicroBlockRollback(toTotalResBlockSig: ByteStr): Unit = {}
  }
}
