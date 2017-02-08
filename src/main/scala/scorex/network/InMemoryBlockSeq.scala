package scorex.network

import scorex.block.Block
import scorex.block.Block._
import scorex.crypto.encode.Base58
import scorex.network.BlockchainSynchronizer.InnerIds
import scorex.transaction.History._

import scala.collection.mutable

class InMemoryBlockSeq(blockIds: InnerIds) {

  private val blocks = mutable.Map.empty[String, Block]

  private val blockIdsSet: Set[String] = blockIds.map(_.toString).toSet

  private def keyToStr(id: BlockId) = Base58.encode(id)

  def addIfNotContained(block: Block): Boolean = {
    blocks.put(keyToStr(block.uniqueId), block).isEmpty
  }

  def noIdsWithoutBlock: Boolean = blockIds.size == blocks.size

  def containsBlockId(blockId: BlockId): Boolean = blockIdsSet.contains(keyToStr(blockId))

  def blocksInOrder: Iterator[Block] = blockIds.
    map(id => blocks.get(id.toString)).
    takeWhile(_.isDefined).
    map(_.get).iterator

  def cumulativeBlockScore(): BlockchainScore =
    blocksInOrder.map(_.blockScore).sum

  def numberOfBlocks: Int = blocks.size
}
