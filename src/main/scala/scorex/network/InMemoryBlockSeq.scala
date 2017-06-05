package scorex.network

import com.wavesplatform.state2.ByteStr
import scorex.block.Block
import scorex.transaction.History._

import scala.collection.mutable

class InMemoryBlockSeq(blockIds: Seq[ByteStr]) {

  private val blocks = mutable.Map.empty[ByteStr, Block]

  private val blockIdsSet: Set[ByteStr] = blockIds.toSet

  def addIfNotContained(block: Block): Boolean = {
    blocks.put(block.uniqueId, block).isEmpty
  }

  def noIdsWithoutBlock: Boolean = blockIds.size == blocks.size

  def containsBlockId(blockId: ByteStr): Boolean = blockIdsSet.contains(blockId)

  def blocksInOrder: Iterator[Block] = blockIds
    .map(blocks.get)
    .takeWhile(_.isDefined)
    .map(_.get)
    .iterator

  def cumulativeBlockScore(): BlockchainScore =
    blocksInOrder.map(_.blockScore).sum

  def numberOfBlocks: Int = blocks.size
}
