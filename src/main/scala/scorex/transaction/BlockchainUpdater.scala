package scorex.transaction

import com.wavesplatform.state2.ByteStr
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
}

trait BlockchainDebugInfo {
  def debugInfo(): StateDebugInfo

  def persistedAccountPortfoliosHash(): Int

}

case class LastBlockInfo(id: BlockId, height: Int, score: BlockchainScore, ready: Boolean)

case class HashInfo(height: Int, hash: Int)

case class StateDebugInfo(persisted: HashInfo,
                          inMemory: Seq[HashInfo],
                          microBaseHash: Option[Int])