package com.wavesplatform.history

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.MicroBlock

class MicroBlockWithTotalId(val microBlock: MicroBlock, val totalBlockId: BlockId)
object MicroBlockWithTotalId {
  implicit def toMicroBlock(mb: MicroBlockWithTotalId): MicroBlock = mb.microBlock
}
