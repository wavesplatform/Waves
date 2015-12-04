package scorex.transaction.state.database.blockchain

import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.ConsensusModule
import scorex.transaction.{BlockTree, History, TransactionModule}
import scorex.utils.ScorexLogging

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.util.Try

/**
  * If no datafolder provided, blocktree lives in RAM (useful for tests)
  */
class StoredBlockTree(dataFolderOpt: Option[String])
                     (implicit consensusModule: ConsensusModule[_],
                      transactionModule: TransactionModule[_])
  extends BlockTree with ScorexLogging {

  private var bestBlockId: BlockId = "".getBytes

  object MemoryBlockTreePersistence {
    type Score = BigInt
    type Height = Int
    type StoredBlock = (Block, Score, Height)
    private val memStorage = TrieMap[BlockId, StoredBlock]()


    def writeBlock(block: Block): Try[Unit] = Try {
      val parent = memStorage.find(_._1.sameElements(block.referenceField.value)).map(_._2)
      lazy val blockScore = consensusModule.blockScore(block).ensuring(_ > 0)
      parent match {
        case Some(p) =>
          val s = p._2 + blockScore
          if (s > score()) bestBlockId = block.uniqueId
          memStorage.put(block.uniqueId, (block, s, p._3 + 1))
        case None => memStorage.isEmpty match {
          case true =>
            bestBlockId = block.uniqueId
            memStorage.put(block.uniqueId, (block, blockScore, 1))
          case false =>
            throw new Error("Parent block is not in tree")
        }
      }
    }

    def readBlock(id: BlockId): Option[StoredBlock] = memStorage.find(_._1.sameElements(id)).map(_._2)

    def filter(f: Block => Boolean): Seq[StoredBlock] = {
      @tailrec
      def iterate(b: StoredBlock, f: Block => Boolean, acc: Seq[StoredBlock] = Seq.empty): Seq[StoredBlock] = {
        val newAcc: Seq[StoredBlock] = if (f(b._1)) b +: acc else acc
        readBlock(b._1.referenceField.value) match {
          case Some(parent) => iterate(parent, f, newAcc)
          case None => newAcc
        }
      }
      iterate(readBlock(bestBlockId).get, f)
    }
  }

  private val blockStorage = dataFolderOpt match {
    case Some(dataFolder) => ???
    case None => MemoryBlockTreePersistence
  }

  /**
    * Height of the a chain, or a longest chain in the explicit block-tree
    */
  override def height(): Int = blockStorage.readBlock(bestBlockId).map(_._3).getOrElse(0)


  /**
    * Use BlockStorage.appendBlock(block: Block) if you want to automatically update state
    *
    * Append block to a chain, based on it's reference
    * @param block - block to append
    * @return Modified version of history
    */
  override private[transaction] def appendBlock(block: Block): Try[History] =
    blockStorage.writeBlock(block).map(x => this)

  override def heightOf(blockId: BlockId): Option[Int] = blockStorage.readBlock(blockId).map(_._3)

  override def blockById(blockId: BlockId): Option[Block] = blockStorage.readBlock(blockId).map(_._1)

  override def generatedBy(account: Account): Seq[Block] = blockStorage.filter { b =>
    consensusModule.generators(b).contains(account)
  }.map(_._1)

  override def lastBlock: Block = blockStorage.readBlock(bestBlockId).map(_._1).get

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    * @return
    */
  override def score(): BigInt = blockStorage.readBlock(bestBlockId).map(_._2).getOrElse(BigInt(0))

  override def parent(block: Block): Option[Block] = blockStorage.readBlock(block.referenceField.value).map(_._1)
}