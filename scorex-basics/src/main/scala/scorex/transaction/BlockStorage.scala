package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage extends ScorexLogging {

  val history: History

  private val stateHistory = TrieMap[String, State]()

  def saveState(id: BlockId, state: State): Unit = stateHistory.put(Base58.encode(id), state)

  def state(id: BlockId): Option[State] = stateHistory.get(Base58.encode(id))

  def state: State = if (history.height() > 0) stateHistory.get(Base58.encode(history.lastBlock.uniqueId)).get
  else genesisState

  val genesisState: State

  //Append block to current state
  def appendBlock(block: Block): Try[Unit] = synchronized {
    history.appendBlock(block).map { blocks =>
      blocks foreach { b =>
        val currentState = if (history.heightOf(b).get != 1) state(b.referenceField.value).get else genesisState
        currentState.processBlock(b) match {
          case Success(st) => saveState(b.uniqueId, st)
          case Failure(e) => log.error("Unable to process block", e)
        }
      }
    }
  }

  //Should be used for linear blockchain only
  def removeAfter(signature: BlockId): Unit = synchronized {
    history match {
      case h: BlockChain =>
        while (!h.lastBlock.uniqueId.sameElements(signature)) h.discardBlock()
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
