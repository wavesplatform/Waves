package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.utils.ScorexLogging

import scala.util.Try

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage extends ScorexLogging {

  val MaxRollback: Int

  val history: History

  val stateHistory: StateHistory

  trait StateHistory {
    def keySet: Set[BlockId]

    def removeState(id: BlockId): Unit

    def copyState(id: BlockId, state: LagonakiState): LagonakiState

    def state(id: BlockId): Option[LagonakiState]

    def state: LagonakiState

    val emptyState: LagonakiState
  }

  def state(id: BlockId): Option[LagonakiState] = stateHistory.state(id)

  def state: LagonakiState = stateHistory.state


  //Append block to current state
  def appendBlock(block: Block): Try[Unit] = synchronized {
    history.appendBlock(block).map { blocks =>
      blocks foreach { b =>
        val cState = if (history.heightOf(b).get != 1) state(b.referenceField.value).get else stateHistory.emptyState
        stateHistory.copyState(b.uniqueId, cState).processBlock(b)
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
