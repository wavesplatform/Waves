package scorex.transaction

import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.HeightInfo
import monix.reactive.Observable
import scorex.block.Block.BlockId
import scorex.block.{Block, MicroBlock}
import scorex.transaction.History.BlockchainScore
import scorex.utils.Synchronized

trait BlockchainUpdater extends Synchronized {

  def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]]

  def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]

  def lastBlockInfo: Observable[LastBlockInfo]

  def shutdown(): Unit
}

trait BlockchainDebugInfo {
  def lockfreeStateHeight: HeightInfo
}

case class LastBlockInfo(id: BlockId, height: Int, score: BlockchainScore, ready: Boolean)
