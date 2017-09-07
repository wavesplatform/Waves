package scorex.transaction

import com.wavesplatform.state2.ByteStr
import scorex.block.{Block, MicroBlock}
import scorex.utils.Synchronized

trait BlockchainUpdater extends Synchronized {

  def processBlock(block: Block): Either[ValidationError, DiscardedTransactions]

  def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedTransactions]
}

trait BlockchainDebugInfo {
  def debugInfo(): StateDebugInfo

  def persistedAccountPortfoliosHash(): Int
}

case class HashInfo(height: Int, hash: Int)

case class StateDebugInfo(persisted: HashInfo,
                          top: HashInfo,
                          bottom: HashInfo,
                          microBaseHash: Option[Int],
                          lastBlockId: String)