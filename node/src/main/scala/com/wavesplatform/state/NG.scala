package com.wavesplatform.state

import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.transaction.Transaction

trait NG {
  def microBlock(totalBlockId: BlockId): Option[MicroBlock]

  def bestLastBlockInfo(maxMicroblockTimestampMs: Long): Option[BlockMinerInfo]

  def microblockIds: Seq[BlockId]

  def liquidBlock(totalBlockId: BlockId): Option[Block]

  def liquidBlockSnapshot(totalBlockId: BlockId): Option[StateSnapshot]

  def microBlockSnapshot(totalBlockId: BlockId): Option[StateSnapshot]

  def liquidTransactions(totalBlockId: BlockId): Option[Seq[(TxMeta, Transaction)]]

  def liquidBlockMeta: Option[BlockMeta]

  def bestLiquidSnapshot: Option[StateSnapshot]

  def bestLiquidSnapshotAndFees: Option[(StateSnapshot, Long, Long)]

  def snapshotBlockchain: SnapshotBlockchain

  def currentGeneratorSet: Option[GeneratorSet]
}
