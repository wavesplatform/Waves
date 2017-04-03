package scorex.transaction

import com.wavesplatform.state2.BlockchainUpdaterImpl
import com.wavesplatform.state2.reader.StateReader
import org.h2.mvstore.MVStore
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage extends ScorexLogging {

  def history: History

  def stateReader: StateReader

  def blockchainUpdater: BlockchainUpdater

  def state: State

  def appendBlock(block: Block): Try[Unit] = {
    blockchainUpdater.processBlock(block).map(_ => ())
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

object BlockStorage {

  type BlocksToProcess = Seq[Block]
}
