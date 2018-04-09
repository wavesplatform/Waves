package com.wavesplatform.state

import scorex.block.Block.BlockId
import scorex.block.MicroBlock

trait NG extends Blockchain {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]

  def lastPersistedBlockIds(count: Int): Seq[BlockId]

  def microblockIds(): Seq[BlockId]
}
