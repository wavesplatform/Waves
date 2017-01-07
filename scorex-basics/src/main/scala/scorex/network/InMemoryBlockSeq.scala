package scorex.network

import scorex.block.Block
import scorex.block.Block._
import scorex.consensus.ConsensusModule
import scorex.crypto.encode.Base58
import scorex.network.BlockchainSynchronizer.{InnerId, InnerIds}
import scorex.transaction.History._
import scorex.transaction.TransactionModule
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

class InMemoryBlockSeq(implicit consensusModule: ConsensusModule[_], transactionModule: TransactionModule[_])
  extends BlockSeq with ScorexLogging {

  private val blocks: TrieMap[String, Block] = TrieMap()

  private var blockIds = mutable.LinkedHashSet.empty[InnerId]
  private var score: BlockchainScore = _

  private def keyToStr(id: BlockId): String = Base58.encode(id)

  override def initialize(ids: InnerIds, initialScore: BlockchainScore): Unit = {
    blocks.clear()
    blockIds.clear()

    blockIds ++= ids
    score = initialScore
  }

  override def allIdsWithoutBlock: InnerIds = blockIds.filterNot(id => blocks.contains(keyToStr(id.blockId))).toSeq

  override def containsBlockId(blockId: BlockId): Boolean = blockIds.contains(InnerId(blockId))

  override def addIfNotContained(block: Block): Boolean = {
    val blockId = block.uniqueId
    assert(containsBlockId(blockId), s"Block ${block.encodedId} is not in the set")
    blocks.putIfAbsent(keyToStr(blockId), block).isEmpty
  }

  override def noIdsWithoutBlock: Boolean = blockIds.size == blocks.size

  def blocksInOrder = blocksIterator

  override def cumulativeBlockScore: BlockchainScore = {
    blocksIterator.foldLeft(score) {
      (sum, block) => ConsensusModule.cumulativeBlockScore(sum, consensusModule.blockScore(block))
    }
  }

  override def numberOfBlocks: Int = blocks.size

  private def blocksIterator = blockIds.toIterator
    .map { innerId => blocks.get(keyToStr(innerId.blockId)) }
    .takeWhile {
      _.isDefined
    }
    .map(bOpt => bOpt.get)

  protected[this] def toBytes(block: Block): Array[Byte] = block.bytes

  protected[this] def fromBytes(bytes: Array[Byte]): Option[Block] = Block.parseBytes(bytes).toOption
}
