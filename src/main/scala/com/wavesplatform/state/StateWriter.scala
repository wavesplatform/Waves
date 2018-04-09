package com.wavesplatform.state

import scorex.block.Block

trait StateWriter {
  def append(diff: Diff, block: Block): Unit
  def rollbackTo(targetBlockId: ByteStr): Seq[Block]
}
