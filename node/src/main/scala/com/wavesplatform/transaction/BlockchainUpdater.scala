package com.wavesplatform.transaction
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, BlockSnapshot, MicroBlock, MicroBlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import monix.reactive.Observable

trait BlockchainUpdater {
  def processBlock(
      block: Block,
      hitSource: ByteStr,
      snapshot: Option[BlockSnapshot],
      challengedHitSource: Option[ByteStr] = None,
      verify: Boolean = true,
      txSignParCheck: Boolean = true
  ): Either[ValidationError, BlockApplyResult]
  def processMicroBlock(
      microBlock: MicroBlock,
      snapshot: Option[MicroBlockSnapshot],
      checkSH: Boolean = true,
      verify: Boolean = true
  ): Either[ValidationError, BlockId]
  def computeNextReward: Option[Long]
  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]
  def lastBlockInfo: Observable[LastBlockInfo]
  def isLastBlockId(id: ByteStr): Boolean
  def shutdown(): Unit
}

case class LastBlockInfo(id: BlockId, height: Int, score: BigInt, ready: Boolean)
