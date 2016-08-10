package scorex.transaction

import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.network.BlockchainSynchronizer.{InnerId, InnerIds}
import scorex.transaction.History.BlockchainScore

trait BlockSeq {

  def initialize(blockIds: InnerIds, initialScore: BlockchainScore)

  def containsBlockId(blockId: BlockId): Boolean

  def firstIdWithoutBlock: Option[InnerId]

  def noIdsWithoutBlock: Boolean

  def addIfNotContained(block: Block): Boolean

  def blocksInOrder: Iterator[Block]

  def cumulativeBlockScore: BlockchainScore

  def numberOfBlocks: Int
}
