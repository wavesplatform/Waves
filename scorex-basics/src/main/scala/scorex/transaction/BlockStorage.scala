package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId

import scala.util.{Success, Failure, Try}

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage {

  //TODO replace BlockChain with History
  val history: BlockChain
  val state: State

  def appendBlock(block: Block): Try[History] = synchronized {
    state.processBlock(block) match {
      case Success(_) => history.appendBlock(block).recoverWith { case t: Throwable =>
        state.processBlock(block, reversal = true)
        Failure(t)
      }
      case Failure(ex) => Failure(ex)
    }
  }

  def removeAfter(signature: BlockId): Unit = synchronized {
    while (!history.lastSignature().sameElements(signature)) {
      val lastBlock = history.lastBlock
      state.processBlock(lastBlock, reversal = true)
      history.discardBlock()
    }
  }


}
