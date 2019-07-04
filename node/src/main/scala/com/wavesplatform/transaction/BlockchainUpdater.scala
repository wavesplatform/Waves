package com.wavesplatform.transaction
import java.io.Closeable

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import monix.reactive.Observable

trait BlockchainUpdater {
  def processBlock(block: Block, verify: Boolean = true): Either[ValidationError, Option[DiscardedTransactions]]
  def processMicroBlock(microBlock: MicroBlock, verify: Boolean = true): Either[ValidationError, Unit]
  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]
  def lastBlockInfo: Observable[LastBlockInfo]
  def isLastBlockId(id: ByteStr): Boolean

  // Compatibility
  @inline def shutdown(): Unit = this match {
    case c: Closeable => c.close()
    case _ => // Ignore
  }
}

case class LastBlockInfo(id: BlockId, height: Int, score: BigInt, ready: Boolean)
