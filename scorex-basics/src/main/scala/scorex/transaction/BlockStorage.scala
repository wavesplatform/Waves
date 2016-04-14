package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage extends ScorexLogging {

  val MaxRollback: Int

  val history: History

  def state: LagonakiState

  //Append block to current state
  def appendBlock(block: Block): Try[Unit] = synchronized {
    history.appendBlock(block).map { blocks =>
      blocks foreach { b =>
        state.processBlock(b) match {
          case Failure(e) =>
            log.error("Failed to apply block to state", e)
            removeAfter(block.referenceField.value)
            //TODO ???
            System.exit(1)
          case Success(m) =>
        }
      }
    }
  }

  //Should be used for linear blockchain only
  def removeAfter(signature: BlockId): Unit = synchronized {
    history match {
      case h: BlockChain => h.heightOf(signature) match {
        case Some(height) =>
          while (!h.lastBlock.uniqueId.sameElements(signature)) h.discardBlock()
          state.rollbackTo(height)
        case None =>
          log.warn(s"RemoveAfter non-existing block ${Base58.encode(signature)}")
      }
      case _ =>
        throw new RuntimeException("Not available for other option than linear blockchain")
    }
  }


}

object BlockStorage {

  sealed trait Direction

  case object Forward extends Direction

  case object Reversed extends Direction

  /*
 * Block and direction to process it
 */
  type BlocksToProcess = Seq[Block]
}
