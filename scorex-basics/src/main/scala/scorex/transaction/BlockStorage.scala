package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage extends ScorexLogging {

  val history: History
  val state: State

  def appendBlock(block: Block): Try[Unit] = synchronized {
    state.processBlock(block) match {
      case Success(newState) =>
        history.appendBlock(block) match {
          case Success(newHistory) => Success()
          case Failure(e) => e match {
            case ShouldBranchFrom(sig) =>
              log.info(s"Better chain detected, rollback to ${sig.mkString}")
              removeAfter(sig)
              appendBlock(block)
            case t: Throwable =>
              state.processBlock(block, reversal = true)
              Failure(t)
          }
        }
      case Failure(ex) => Failure(ex)
    }
  }

  //Should be used for linear blockchain only
  def removeAfter(signature: BlockId): Unit = synchronized {
    history match {
      case h: BlockChain =>
        while (!h.lastSignature().sameElements(signature)) {
          state.processBlock(history.lastBlock, reversal = true)
          h.discardBlock()
        }
      case _ =>
        throw new RuntimeException("Not available for other option than linear blockchain")
    }
  }


}

case class ShouldBranchFrom(s: BlockId) extends Error
