package scorex.transaction

import com.wavesplatform.state2.{ByteStr, Portfolio}
import monix.reactive.Observable
import scorex.account.Address
import scorex.block.{Block, MicroBlock}
import scorex.utils.Synchronized

trait BlockchainUpdater extends Synchronized {

  def processBlock(block: Block): Either[ValidationError, Option[DiscardedTransactions]]

  def processMicroBlock(microBlock: MicroBlock): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]

  def lastBlockId: Observable[ByteStr]
}

trait BlockchainDebugInfo {
  def debugInfo(): StateDebugInfo

  def persistedAccountPortfoliosHash(): Int

  def topDiff(): Map[Address, Portfolio]

  def bottomDiff(): Map[Address, Portfolio]
}

case class HashInfo(height: Int, hash: Int)

case class StateDebugInfo(persisted: HashInfo,
                          inMemory: Seq[HashInfo],
                          microBaseHash: Option[Int])