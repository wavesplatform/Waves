package scorex.transaction.state.database.blockchain

import java.io.File

import org.mapdb.{DBMaker, HTreeMap, Serializer}
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.ConsensusModule
import scorex.transaction.BlockStorage._
import scorex.transaction.{BlockTree, TransactionModule}
import scorex.utils.ScorexLogging

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

/**
  * If no datafolder provided, blocktree lives in RAM (useful for tests)
  */
class StoredBlockTree(dataFolderOpt: Option[String], MaxRollback: Int = 100)
                     (implicit consensusModule: ConsensusModule[_],
                      transactionModule: TransactionModule[_])
  extends BlockTree with ScorexLogging {

  trait BlockTreePersistence {
    type Score = BigInt
    type Height = Int
    type StoredBlock = (Block, Score, Height)

    def writeBlock(block: Block): Try[Boolean]

    def readBlock(id: BlockId): Option[StoredBlock]

    def filter(f: Block => Boolean): Seq[StoredBlock] = {
      @tailrec
      def iterate(b: StoredBlock, f: Block => Boolean, acc: Seq[StoredBlock] = Seq.empty): Seq[StoredBlock] = {
        val newAcc: Seq[StoredBlock] = if (f(b._1)) b +: acc else acc
        readBlock(b._1.referenceField.value) match {
          case Some(parent) => iterate(parent, f, newAcc)
          case None => newAcc
        }
      }
      iterate(readBlock(getBestBlockId).get, f)
    }

    def exists(block: Block): Boolean = exists(block.uniqueId)

    def exists(blockId: BlockId): Boolean = readBlock(blockId).isDefined

    def bestBlock: Option[StoredBlock] = readBlock(getBestBlockId)

    protected def getBestBlockId: BlockId

  }

  //TODO remove old blocks
  class FileBlockTreePersistence(folder: String) extends BlockTreePersistence {
    type MapDBStoredBlock = (Array[Byte], Score, Height)

    new File(folder).mkdirs()
    private val file = new File(folder + "blocktree.mapDB")

    private lazy val db =
      DBMaker.appendFileDB(file)
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .checksumEnable()
        .make()

    private lazy val map: HTreeMap[BlockId, MapDBStoredBlock] = db.hashMapCreate("segments")
      .keySerializer(Serializer.BYTE_ARRAY).makeOrGet()

    private lazy val bestBlockStorage: HTreeMap[Int, BlockId] = db.hashMapCreate("bestBlock")
      .keySerializer(Serializer.INTEGER).valueSerializer(Serializer.BYTE_ARRAY).makeOrGet()

    private var bestBlockId: BlockId = Option(bestBlockStorage.get(0)).getOrElse(Array.empty)

    override protected def getBestBlockId: BlockId = bestBlockId

    private def setBestBlockId(newId: BlockId) = {
      bestBlockId = newId
      bestBlockStorage.put(0, newId)
    }

    override def writeBlock(block: Block): Try[Boolean] = Try {
      if (exists(block)) throw new Error("Block is already in storage")
      val parent = readBlock(block.referenceField.value)
      lazy val blockScore = consensusModule.blockScore(block).ensuring(_ > 0)
      parent match {
        case Some(p) =>
          val s = p._2 + blockScore
          map.put(block.uniqueId, (block.bytes, s, p._3 + 1))
          db.commit()
          if (s > score()) {
            setBestBlockId(block.uniqueId)
            true
          } else false
        case None => map.isEmpty match {
          case true =>
            setBestBlockId(block.uniqueId)
            map.put(block.uniqueId, (block.bytes, blockScore, 1))
            db.commit()
            true
          case false =>
            throw new Error(s"Parent ${block.referenceField.value.mkString} block is not in tree")
        }
      }
    }

    override def exists(blockId: BlockId): Boolean = map.containsKey(blockId)

    override def readBlock(key: BlockId): Option[StoredBlock] = Try {
      val stored = map.get(key)
      (Block.parse(stored._1).get, stored._2, stored._3)
    } match {
      case Success(v) =>
        Some(v)
      case Failure(e) =>
        log.debug("Enable readBlock for key: " + key.mkString)
        None
    }
  }

  object MemoryBlockTreePersistence extends BlockTreePersistence {
    private val memStorage = TrieMap[BlockId, StoredBlock]()

    private var bestBlockId: BlockId = Array.empty

    private def setBestBlockId(newId: BlockId) = bestBlockId = newId

    override def getBestBlockId: BlockId = bestBlockId

    def writeBlock(block: Block): Try[Boolean] = Try {
      if (exists(block)) throw new Error("Block is already in storage")
      val parent = readBlock(block.referenceField.value)
      lazy val blockScore = consensusModule.blockScore(block).ensuring(_ > 0)
      parent match {
        case Some(p) =>
          val s = p._2 + blockScore
          memStorage.put(block.uniqueId, (block, s, p._3 + 1))
          if (s > score()) {
            setBestBlockId(block.uniqueId)
            true
          } else false
        case None => memStorage.isEmpty match {
          case true =>
            setBestBlockId(block.uniqueId)
            memStorage.put(block.uniqueId, (block, blockScore, 1))
            true
          case false =>
            throw new Error("Parent block is not in tree")
        }
      }
    }

    def readBlock(id: BlockId): Option[StoredBlock] = memStorage.find(_._1.sameElements(id)).map(_._2)

  }

  private val blockStorage: BlockTreePersistence = dataFolderOpt match {
    case Some(dataFolder) => new FileBlockTreePersistence(dataFolder)
    case None => MemoryBlockTreePersistence
  }

  /**
    * Height of the a chain, or a longest chain in the explicit block-tree
    */
  override def height(): Int = blockStorage.bestBlock.map(_._3).getOrElse(0)


  /**
    * Use BlockStorage.appendBlock(block: Block) if you want to automatically update state
    *
    * Append block to a chain, based on it's reference
    * @param block - block to append
    * @return Modified version of history
    */
  override private[transaction] def appendBlock(block: Block): Try[BlocksToProcess] = {
    val parent = block.referenceField
    val h = height()
    if ((h == 0) || (lastBlock.uniqueId sameElements block.referenceField.value)) {
      blockStorage.writeBlock(block)
      Success(Seq((block, Forward)))
    } else blockById(parent.value) match {
      case Some(commonBlock) =>
        lazy val oldLast = lastBlock
        blockStorage.writeBlock(block) map {
          case true =>
            branchBlock(oldLast, block, MaxRollback) match {
              case Some(node) =>
                val toReverse = lastBlocks(oldLast, heightOf(oldLast).get - heightOf(node).get + 1).map((_, Reversed))
                val toProcess = lastBlocks(block, heightOf(block).get - heightOf(node).get + 1).map((_, Forward))
                toReverse ++ toProcess
              case None => ??? //Should never rich this point if we don't keep older then MaxRollback side chains
            }
          case false => Seq.empty
        }
      case None => Failure(new Error(s"Appending block ${block.json} which parent is not in block tree"))
    }
  }

  def branchBlock(b1: Block, b2: Block, in: Int): Option[Block] = {
    val b1LastBlocks = lastBlocks(b1, in)
    find(b2, in)(b1LastBlocks.contains(_))
  }

  override def heightOf(blockId: BlockId): Option[Int] = blockStorage.readBlock(blockId).map(_._3)

  override def blockById(blockId: BlockId): Option[Block] = blockStorage.readBlock(blockId).map(_._1)

  override def generatedBy(account: Account): Seq[Block] = blockStorage.filter { b =>
    consensusModule.generators(b).contains(account)
  }.map(_._1)

  override def lastBlock: Block = blockStorage.bestBlock.map(_._1).get

  def find(block: Block, limit: Int)(condition: Block => Boolean): Option[Block] = if (limit > 0) {
    parent(block) match {
      case Some(pb) =>
        if (condition(pb)) Some(pb)
        else find(pb, limit - 1)(condition)
      case None => None
    }
  } else None

  def lastBlocks(block: Block, howMany: Int): Seq[Block] = {
    def loop(block: Block, i: Int, acc: Seq[Block] = Seq.empty): Seq[Block] = {
      lazy val p = parent(block)
      (i, p) match {
        case (0, _) => acc
        case (m, Some(parentBlock)) => loop(parentBlock, i - 1, parentBlock +: acc)
        case _ => acc
      }
    }
    loop(block, howMany)
  }

  override def lastBlocks(howMany: Int): Seq[Block] = {
    lastBlocks(lastBlock, howMany).reverse
  }

  /**
    * Quality score of a best chain, e.g. cumulative difficulty in case of Bitcoin / Nxt
    * @return
    */
  override def score(): BigInt = blockStorage.bestBlock.map(_._2).getOrElse(BigInt(0))

  override def parent(block: Block, back: Int = 1): Option[Block] = {
    require(back > 0)
    val p = blockStorage.readBlock(block.referenceField.value).map(_._1)
    (back, p) match {
      case (1, _) => p
      case (m, Some(parentBlock)) => parent(parentBlock, m - 1)
      case _ => None
    }
  }

  override def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId] = ???

  override def contains(id: BlockId): Boolean = blockStorage.exists(id)

}
