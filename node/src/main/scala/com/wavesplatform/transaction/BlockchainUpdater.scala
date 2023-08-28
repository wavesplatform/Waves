package com.wavesplatform.transaction
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import monix.reactive.Observable

trait BlockchainUpdater {
  def processBlock(
      block: Block,
      hitSource: ByteStr,
      challengedHitSource: Option[ByteStr] = None,
      verify: Boolean = true,
      txSignParCheck: Boolean = true,
      checkStateHash: Boolean = true // TODO: remove after NODE-2568 merge (at NODE-2609)
  ): Either[ValidationError, BlockApplyResult]
  def processMicroBlock(microBlock: MicroBlock, verify: Boolean = true): Either[ValidationError, BlockId]
  def computeNextReward: Option[Long]
  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]
  def lastBlockInfo: Observable[LastBlockInfo]
  def isLastBlockId(id: ByteStr): Boolean
  def shutdown(): Unit
}

case class LastBlockInfo(id: BlockId, height: Int, score: BigInt, ready: Boolean)
