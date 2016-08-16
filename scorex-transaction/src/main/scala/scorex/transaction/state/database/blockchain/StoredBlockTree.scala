package scorex.transaction.state.database.blockchain

import java.io.File

import org.mapdb.{DB, DBMaker, HTreeMap, Serializer}
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.ConsensusModule
import scorex.crypto.encode.Base58
import scorex.transaction.BlockStorage._
import scorex.transaction.{BlockTree, TransactionModule}
import scorex.utils.ScorexLogging

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
 * TODO fix situation of forks
 * If no datafolder provided, blocktree lives in RAM (useful for tests)
 */
class StoredBlockTree(dataFolderOpt: Option[String], MaxRollback: Int)
                     (implicit consensusModule: ConsensusModule[_],
                      transactionModule: TransactionModule[_])
  extends BlockTree with ScorexLogging {

  trait BlockTreePersistence {
    type Score = BigInt
    type Height = Int
    type StoredBlock = (Block, Score, Height)

    def writeBlock(block: Block): Try[Boolean]

    def readBlock(id: BlockId): Option[StoredBlock]

    def readBlock(block: Block): Option[StoredBlock] = readBlock(block.uniqueId)

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

    def changeBestChain(changes: Seq[(Block, Direction)]): Try[Unit]

    def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId]

  }

  //TODO remove old blocks
  class MapDBBlockTreePersistence(db: DB) extends BlockTreePersistence {
    type MapDBStoredBlock = (Array[Byte], Score, Height)

    private lazy val map: HTreeMap[BlockId, MapDBStoredBlock] = db.hashMapCreate("blocks")
      .keySerializer(Serializer.BYTE_ARRAY).makeOrGet()

    private lazy val bestBlockStorage: HTreeMap[Int, BlockId] = db.hashMapCreate("bestBlock")
      .keySerializer(Serializer.INTEGER).valueSerializer(Serializer.BYTE_ARRAY).makeOrGet()

    private lazy val bestChainStorage: HTreeMap[BlockId, (BlockId, Option[BlockId])] = db.hashMapCreate("bestChain")
      .keySerializer(Serializer.BYTE_ARRAY).makeOrGet()

    private var bestBlockId: BlockId = Option(bestBlockStorage.get(0)).getOrElse(Array.empty)

    override protected def getBestBlockId: BlockId = bestBlockId

    private def setBestBlockId(newId: BlockId) = {
      bestBlockId = newId
      bestBlockStorage.put(0, newId)
      db.commit()
    }

    override def lookForward(parentSignature: BlockId, howMany: Height): Seq[BlockId] = Try {
      def loop(parentSignature: BlockId, howMany: Height, acc: Seq[BlockId]): Seq[BlockId] = howMany match {
        case 0 => acc
        case _ =>
          Option(bestChainStorage.get(parentSignature)) match {
            case Some(block) =>
              block._2 match {
                case Some(blockId) => loop(blockId, howMany - 1, blockId +: acc)
                case None => acc
              }
            case None =>
              log.error(s"Failed to get block ${parentSignature.mkString} from best chain storage")
              acc
          }
      }

      loop(parentSignature, howMany, Seq.empty).reverse
    }.recoverWith { case t: Throwable =>
      log.error("Error when getting blocks", t)
      t.printStackTrace()
      Try(Seq.empty)
    }.getOrElse(Seq.empty)

    override def changeBestChain(changes: Seq[(Block, Direction)]): Try[Unit] = Try {
      changes.map { c =>
        val parentId = c._1.referenceField.value
        c._2 match {
          case Forward =>
            bestChainStorage.put(c._1.uniqueId, (parentId, None))
            val prev = bestChainStorage.get(parentId)
            bestChainStorage.put(parentId, (prev._1, Some(c._1.uniqueId)))
          case Reversed =>
            bestChainStorage.remove(c._1.uniqueId)
            val prev = bestChainStorage.get(parentId)
            bestChainStorage.put(parentId, (prev._1, None))
        }
      }
    }

    /**
     *
     * @return true when best block added, false when block score is less then current score
     */
    override def writeBlock(block: Block): Try[Boolean] = Try {
      if (exists(block)) log.warn(s"Trying to add block ${block.encodedId} that is already in tree "
        + s" at height ${readBlock(block).map(_._3)}")
      val parent = readBlock(block.referenceField.value)
      lazy val blockScore = consensusModule.blockScore(block).ensuring(_ > 0)
      parent match {
        case Some(p) =>
          if (height() - p._3 > MaxRollback) {
            throw new Error(s"Trying to add block with too old parent")
          } else {
            val s = ConsensusModule.cumulativeBlockScore(p._2, blockScore)
            map.put(block.uniqueId, (block.bytes, s, p._3 + 1))
            db.commit()
            if (s >= score()) {
              setBestBlockId(block.uniqueId)
              true
            } else false
          }
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
      (Block.parseBytes(stored._1).get, stored._2, stored._3)
    } match {
      case Success(v) =>
        Some(v)
      case Failure(e) =>
        log.debug("Unable readBlock for key: " + Base58.encode(key))
        None
    }
  }

  private val blockStorage: BlockTreePersistence = dataFolderOpt match {
    case Some(dataFolder) =>
      new File(dataFolder).mkdirs()
      val file = new File(dataFolder + "/blocktree.mapDB")
      val db = DBMaker.appendFileDB(file).fileMmapEnableIfSupported().closeOnJvmShutdown().checksumEnable().make()
      new MapDBBlockTreePersistence(db)
    case _ => new MapDBBlockTreePersistence(DBMaker.memoryDB().make())
  }

  override def height(): Int = blockStorage.bestBlock.map(_._3).getOrElse(0)

  override def appendBlock(block: Block): Try[BlocksToProcess] = {
    val parent = block.referenceField
    val h = height()
    if ((h == 0) || (lastBlock.uniqueId sameElements block.referenceField.value)) {
      blockStorage.changeBestChain(Seq((block, Forward)))
      blockStorage.writeBlock(block).map(x => Seq(block))
    } else blockById(parent.value) match {
      case Some(commonBlock) =>
        val oldLast = lastBlock
        blockStorage.writeBlock(block) map {
          case true =>
            branchBlock(oldLast, block, MaxRollback) match {
              case Some(node) =>
                val toReverse = oldLast +: lastBlocks(oldLast, heightOf(oldLast).get - heightOf(node).get - 1)
                val toProcess = block +: lastBlocks(block, heightOf(block).get - heightOf(node).get - 1)
                val stateChanges = toReverse.map((_, Reversed)) ++ toProcess.map((_, Forward))
                blockStorage.changeBestChain(stateChanges)
                toProcess
              case None => ??? //Should never rich this point if we don't keep older then MaxRollback side chains
            }
          case false => Seq.empty
        }
      case None => Failure(new Error(s"Appending block ${block.json} which parent is not in block tree"))
    }
  }

  def branchBlock(b1: Block, b2: Block, in: Int): Option[Block] = {
    val b1LastBlocks = lastBlocks(b1, in)
    find(b2, in)(b => b1LastBlocks.exists(x => x.uniqueId sameElements b.uniqueId))
  }

  override def heightOf(blockId: BlockId): Option[Int] = blockStorage.readBlock(blockId).map(_._3)

  override def blockById(blockId: BlockId): Option[Block] = blockStorage.readBlock(blockId).map(_._1)

  override def generatedBy(account: Account, from: Int, to: Int): Seq[Block] = blockStorage.filter { b =>
    heightOf(b).exists(bh => bh > from && bh < to) && consensusModule.generators(b).contains(account)
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
    require(howMany >= 0)
    def loop(block: Block, i: Int, acc: Seq[Block] = Seq.empty): Seq[Block] = {
      lazy val p = parent(block)
      (i, p) match {
        case (0, _) => acc
        case (m, Some(parentBlock)) => loop(parentBlock, i - 1, parentBlock +: acc)
        case _ => acc
      }
    }
    loop(block, howMany, Seq(block))
  }

  override def lastBlocks(howMany: Int): Seq[Block] = {
    lastBlocks(lastBlock, howMany).reverse
  }

  override def score(): BigInt = blockStorage.bestBlock.map(_._2).getOrElse(BigInt(0))

  def scoreOf(id: BlockId): BigInt = ??? // TODO to be implemented

  override def parent(block: Block, back: Int = 1): Option[Block] = {
    require(back > 0)
    val p = blockStorage.readBlock(block.referenceField.value).map(_._1)
    (back, p) match {
      case (1, _) => p
      case (m, Some(parentBlock)) => parent(parentBlock, m - 1)
      case _ => None
    }
  }

  override def lookForward(parentSignature: BlockId, howMany: Int): Seq[BlockId] =
    blockStorage.lookForward(parentSignature, howMany)

  override def contains(id: BlockId): Boolean = blockStorage.exists(id)

  override lazy val genesis: Block = blockById(Block.genesis().uniqueId).get

}
