package scorex.network

import scorex.block.Block
import scorex.block.Block._
import scorex.network.BlockchainSynchronizer.InnerIds
import scorex.transaction.History._

trait BlockSeq {

  def initialize(blockIds: InnerIds, initialScore: BlockchainScore)

  def containsBlockId(blockId: BlockId): Boolean

  def firstIdsWithoutBlock(n: Int): InnerIds

  def noIdsWithoutBlock: Boolean

  def addIfNotContained(block: Block): Boolean

  def blocksInOrder: Iterator[Block]

  def cumulativeBlockScore: BlockchainScore

  def numberOfBlocks: Int
}
