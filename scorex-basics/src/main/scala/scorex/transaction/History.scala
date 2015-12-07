package scorex.transaction

import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId

import scala.util.Try

/**
  * History of a blockchain system is some blocktree in fact(like this: http://image.slidesharecdn.com/sfbitcoindev-chepurnoy-2015-150322043044-conversion-gate01/95/proofofstake-its-improvements-san-francisco-bitcoin-devs-hackathon-12-638.jpg),
  * where longest chain is being considered as canonical one, containing right kind of history.
  *
  * In cryptocurrencies of today blocktree view is usually implicit, means code supports only linear history,
  * but other options are possible.
  *
  * To say "longest chain" is the canonical one is simplification, usually some kind of "cumulative difficulty"
  * function has been used instead, even in PoW systems.
  */

trait History {

  /**
    * Height of the a chain, or a longest chain in the explicit block-tree
    */
  def height(): Int

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    * @return
    */
  def score(): BigInt

  /**
    * Is there's no history, even genesis block
    * @return
    */
  def isEmpty: Boolean = height() == 0

  def contains(block: Block): Boolean = contains(block.uniqueId)

  def contains(id: BlockId): Boolean = blockById(id).isDefined

  def blockById(blockId: Block.BlockId): Option[Block]

  /**
    * Height of a block if it's in the blocktree
    */
  def heightOf(block: Block): Option[Int] = heightOf(block.uniqueId)

  def heightOf(blockId: Block.BlockId): Option[Int]

  /**
    * Use BlockStorage.appendBlock(block: Block) if you want to automatically update state
    *
    * Append block to a chain, based on it's reference
    * @param block - block to append
    * @return Modified version of history
    */
  private[transaction] def appendBlock(block: Block): Try[History]

  def parent(block: Block, back: Int = 1): Option[Block]

  def confirmations(block: Block): Option[Int] =
    heightOf(block).map(height() - _)

  def generatedBy(account: Account): Seq[Block]

  /**
    * Block with maximum blockchain score
    */
  def lastBlock: Block

  /**
    * Average delay in milliseconds between last $blockNum blocks starting from $block
    */
  def averageDelay(block: Block, blockNum: Int): Try[Long] = Try {
    (block.timestampField.value - parent(block, blockNum).get.timestampField.value) / blockNum
  }

}