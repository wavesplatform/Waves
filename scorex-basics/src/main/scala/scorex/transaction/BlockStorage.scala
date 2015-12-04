package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage extends ScorexLogging {

  //TODO replace BlockChain with History
  val history: BlockChain
  val state: State

  def appendBlock(block: Block): Try[History] = synchronized {
    state.processBlock(block) match {
      case Success(_) => history.appendBlock(block).recoverWith {
        case ShouldBranchFrom(sig) =>
          log.info(s"Better chain detected, rollback to ${sig.mkString}")
          removeAfter(sig)
          appendBlock(block)
        case t: Throwable =>
          state.processBlock(block, reversal = true)
          Failure(t)
      }
      case Failure(ex) => Failure(ex)
    }
  }

  def removeAfter(signature: BlockId): Unit = synchronized {
    while (!history.lastSignature().sameElements(signature)) {
      state.processBlock(history.lastBlock, reversal = true)
      history.discardBlock()
    }
  }


}

case class ShouldBranchFrom(s: BlockId) extends Error
