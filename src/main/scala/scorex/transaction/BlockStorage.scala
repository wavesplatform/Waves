package scorex.transaction

import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.util.Try

trait BlockStorage extends ScorexLogging {

  def history: History

  def stateReader: StateReader

  def blockchainUpdater: BlockchainUpdater

  def appendBlock(block: Block): Try[Unit] = {
    blockchainUpdater.processBlock(block)
  }

  def removeAfter(blockId: BlockId): Unit = try {
    history.heightOf(blockId) match {
      case Some(height) =>
        blockchainUpdater.rollbackTo(height)
      case None =>
        log.warn(s"RemoveAfter non-existing block ${Base58.encode(blockId)}")
    }
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      None
  }
}
