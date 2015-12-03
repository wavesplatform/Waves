package scorex.transaction

import scorex.block.Block

import scala.util.Try

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
trait BlockStorage {

  //TODO replace BlockChain with History
  val history: BlockChain
  val state: State

  def appendBlock(block: Block): Try[Unit] = synchronized {
    state.processBlock(block)
    history.appendBlock(block)
    Try()
  }

}
