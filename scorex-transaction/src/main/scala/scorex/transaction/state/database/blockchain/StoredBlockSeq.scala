package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.block.Block
import scorex.block.Block._
import scorex.consensus.ConsensusModule
import scorex.network.BlockchainSynchronizer.{InnerId, InnerIds}
import scorex.transaction.History._
import scorex.transaction.{BlockSeq, TransactionModule}
import scorex.utils.ScorexLogging

import scala.collection.mutable

class StoredBlockSeq(db: MVStore)
                    (implicit consensusModule: ConsensusModule[_], transactionModule: TransactionModule[_])
  extends BlockSeq with ScorexLogging {

  private val blocks: MVMap[BlockId, Array[Byte]] = db.openMap("forkBlocks")

  private var blockIds = mutable.LinkedHashSet.empty[InnerId]
  private var score: BlockchainScore = _

  override def initialize(ids: InnerIds, initialScore: BlockchainScore): Unit = {
    blocks.clear()
    blockIds.clear()

    blockIds ++= ids
    score = initialScore
  }

  override def firstIdWithoutBlock: Option[InnerId] = blockIds.filterNot(id => blocks.containsKey(id.blockId)).headOption

  override def containsBlockId(blockId: BlockId): Boolean = blockIds.contains(InnerId(blockId))

  override def addIfNotContained(block: Block): Boolean = {
    val blockId = block.uniqueId
    assert(containsBlockId(blockId), s"Block ${block.encodedId} is not in the set")
    blocks.putIfAbsent(blockId, toBytes(block)) == null
  }

  override def noIdsWithoutBlock: Boolean = blockIds.size == blocks.size

  def blocksInOrder = blocksIterator

  override def cumulativeBlockScore: BlockchainScore = {
    blocksIterator.foldLeft(score) {
      (sum, block) => ConsensusModule.cumulativeBlockScore(sum, consensusModule.blockScore(block))
    }
  }

  override def numberOfBlocks: Int = blocks.size()

  private def blocksIterator = blockIds.toIterator
    .map { innerId => blocks.get(innerId.blockId) }
    .takeWhile { _ != null }
    .map( bytes => fromBytes(bytes).get)

  protected[this] def toBytes(block: Block): Array[Byte] = block.bytes
  protected[this] def fromBytes(bytes: Array[Byte]): Option[Block] = Block.parseBytes(bytes).toOption
}
