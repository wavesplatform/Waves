package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.network.BlockchainSynchronizer.InnerIds
import scorex.transaction.History.BlockchainScore

trait BlockSeq {

  def initialize(blockIds: InnerIds, initialScore: BlockchainScore)

  def containsBlockId(blockId: BlockId): Boolean

  def idsWithoutBlock: InnerIds

  def noIdsWithoutBlock: Boolean

  def addIfNotContained(block: Block): Boolean

  def blocksInOrder: Iterator[Block]

  def cumulativeBlockScore: BlockchainScore

  def numberOfBlocks: Int
}
