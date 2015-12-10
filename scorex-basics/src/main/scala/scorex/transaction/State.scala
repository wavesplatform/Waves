package scorex.transaction

import scorex.block.Block
import scorex.transaction.BlockStorage.{Direction, Forward, Reversed}

import scala.util.Try

/**
  * Abstract functional interface of state which is a result of a sequential blocks applying
  */
trait State {
  private[transaction] def processBlock(block: Block, reversal: Boolean): Try[State]

  private[transaction] def processBlock(block: Block, direction: Direction): Try[State] = direction match {
    case Forward => processBlock(block, reversal = false)
    case Reversed => processBlock(block, reversal = true)
  }

  private[transaction] def processBlock(block: Block): Try[State] = processBlock(block, reversal = false)


  /**
    * State is needed to determine whether a transaction was already processed or not.
    * Otherwise, full blockchain scan or storing unique transaction ids is needed and both methods
    * are impractical
    * @param tx - a transaction to check
    * @return whether transaction is already included into a state or not
    */
  //TODO: implement
  def included(tx: Transaction):Boolean = ???
}
