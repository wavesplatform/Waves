package com.wavesplatform.state

import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Transaction

trait NG {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]

  def microblockIds: Seq[BlockId]

  def liquidBlock(id: ByteStr): Option[Block]

  def liquidBlockSnapshot(id: ByteStr): Option[StateSnapshot]

  def microBlockSnapshot(totalBlockId: ByteStr): Option[StateSnapshot]

  def liquidTransactions(id: ByteStr): Option[Seq[(TxMeta, Transaction)]]

  def liquidBlockMeta: Option[BlockMeta]

  def bestLiquidSnapshot: Option[StateSnapshot]

  def bestLiquidSnapshotAndFees: Option[(StateSnapshot, Long, Long)]

  def snapshotBlockchain: SnapshotBlockchain

  def currentGeneratorBalances: Option[GeneratorBalances]
}
