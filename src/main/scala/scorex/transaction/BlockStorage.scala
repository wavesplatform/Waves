package scorex.transaction

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

  def state: LagonakiState

  //Append block to current state
  def appendBlock(block: Block): Try[Unit] = {
    //TODO Rollback state for blocktree
    history.appendBlock(block).map { blocks =>
      blocks foreach { b =>
        state.processBlock(b) match {
          case Failure(e) =>
            log.error("Failed to apply block to state", e)
            db.rollback()
            throw e
          case Success(m) =>
            db.commit()
        }
      }
    } recoverWith { case e =>
      log.error("Failed to append block:", e)
      Failure(e)
    }
  }

  //Should be used for linear blockchain only
  def removeAfter(blockId: BlockId): Unit = try {
    history match {
      case h: BlockChain => h.heightOf(blockId) match {
        case Some(height) =>
          while (!h.lastBlock.uniqueId.sameElements(blockId)) h.discardBlock()
          state.rollbackTo(height)
        case None =>
          log.warn(s"RemoveAfter non-existing block ${Base58.encode(blockId)}")
      }
      case _ =>
        // Not available for other option than linear blockchain
    }
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      None
  }

  protected[this] val db: MVStore
}

object BlockStorage {

  sealed trait Direction

  case object Forward extends Direction

  case object Reversed extends Direction

  /**
    * Block and direction to process it
    */
  type BlocksToProcess = Seq[Block]
}
