package scorex.transaction.state.database.blockchain

import better.files._
import org.mapdb.DBMaker
import scorex.account.Account
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.transaction.BlockStorage._
import scorex.transaction.{BlockChain, TransactionModule}
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

/**
  * If no datafolder provided, blockchain lives in RAM (useful for tests)
  */
class StoredBlockchain(dataFolderOpt: Option[String])
                      (implicit consensusModule: ConsensusModule[_],
                       transactionModule: TransactionModule[_])
  extends BlockChain with ScorexLogging {

  trait BlockchainPersistence {
    def writeBlock(height: Int, block: Block): Try[Unit]

    def readBlock(height: Int): Option[Block]

    def deleteBlock(height: Int): Unit
  }

  case class FileBlockchainPersistence(dataFolder: String) extends BlockchainPersistence {
    private def blockFile(height: Int) = File(dataFolder + s"/block-$height")

    override def writeBlock(height: Int, block: Block): Try[Unit] = Try {
      val blockBytes = block.bytes
      blockFile(height)
        .clear()
        .write(blockBytes)
        .ensuring(_.size == blockBytes.length)
    }

    override def readBlock(height: Int): Option[Block] = {
      Try(blockFile(height).byteArray)
        .flatMap(bs => Block.parse(bs))
        .recoverWith { case t =>
          log.error(s"Error while reading a block for height $height", t)
          Failure(t)
        }.toOption
    }

    override def deleteBlock(height: Int): Unit =
      Try(blockFile(height).delete()).recover { case t =>
        log.error(s"Can't delete blockfile: ${blockFile(height).name}", t)
      }
  }

  object MemoryBlockchainPersistence$ extends BlockchainPersistence {
    private val memStorage = TrieMap[Int, Block]()

    override def writeBlock(height: Int, block: Block): Try[Unit] =
      Success(memStorage.put(height, block))

    override def readBlock(height: Int): Option[Block] = memStorage.get(height)

    override def deleteBlock(height: Int): Unit = memStorage.remove(height)
  }

  private val (blockStorage, database) = dataFolderOpt match {
    case Some(dataFolder) =>
      (FileBlockchainPersistence(dataFolder),
        DBMaker.appendFileDB(new java.io.File(dataFolder + s"/signatures"))
          .fileMmapEnableIfSupported()
          .closeOnJvmShutdown()
          .checksumEnable()
          .make())
    case None =>
      (MemoryBlockchainPersistence$, DBMaker.memoryDB().make())
  }

  private val signaturesIndex = database.treeMap[Int, Array[Byte]]("signatures")

  //if there are some uncommited changes from last run, discard'em
  if (signaturesIndex.size() > 0) database.rollback()

  override private[transaction] def appendBlock(block: Block): Try[BlocksToProcess] = synchronized {
    Try {
      val parent = block.referenceField
      if ((height() == 0) || (lastBlock.uniqueId sameElements parent.value)) {
        val h = height() + 1
        blockStorage.writeBlock(h, block).flatMap(_ => Try(signaturesIndex.put(h, block.uniqueId))) match {
          case Success(_) =>
            database.commit()
            Seq((block, Forward))
          case Failure(t) => throw new Error("Error while storing blockchain a change: " + t)
        }
      } else blockById(parent.value) match {
        case Some(commonBlock) =>
          val branchPoint = heightOf(commonBlock).get
          val blockScore = consensusModule.blockScore(block)
          val blocksFromBranchPoint = ((branchPoint + 1) to height()).map(i => blockAt(i).get).reverse
          val currentScore = blocksFromBranchPoint.map(b => consensusModule.blockScore(b)).sum
          if (blockScore > currentScore) {
            val toRollback = blocksFromBranchPoint map { b =>
              val h = heightOf(b).get
              blockStorage.deleteBlock(h)
              signaturesIndex.remove(h)
              (b, Reversed)
            }
            val h = height() + 1
            blockStorage.writeBlock(h, block).flatMap(_ => Try(signaturesIndex.put(h, block.uniqueId))) match {
              case Success(_) =>
                database.commit()
                toRollback ++ Seq((block, Forward))
              case Failure(t) =>
                database.rollback()
                throw new Error("Error while storing blockchain a change: " + t)
            }
          } else Seq.empty
        case None => throw new Error(s"Appending block ${block.json} which parent is not in blockchain")
      }
    }
  }


  override private[transaction] def discardBlock(): BlockChain = synchronized {
    require(height() > 1, "Chain is empty or contains genesis block only, can't make rollback")
    val h = height()
    blockStorage.deleteBlock(h)
    signaturesIndex.remove(h)
    database.commit()
    this
  }

  override def heightOf(block: Block): Option[Int] =
    signaturesIndex.descendingMap().find(_._2.sameElements(block.uniqueId)).map(_._1)

  override def blockAt(height: Int): Option[Block] = synchronized {
    blockStorage.readBlock(height)
  }

  override def contains(signature: Array[Byte]): Boolean =
    signaturesIndex.exists(_._2.sameElements(signature))

  override def height(): Int = signaturesIndex.size

  override def heightOf(blockSignature: Array[Byte]): Option[Int] =
    signaturesIndex.find(_._2.sameElements(blockSignature)).map(_._1)

  override def blockById(blockId: Block.BlockId): Option[Block] =
    heightOf(blockId).flatMap(blockAt)

  override def children(block: Block): Seq[Block] =
    heightOf(block).flatMap(h => blockAt(h + 1)).toSeq

  override def generatedBy(account: Account): Seq[Block] =
    (1 to height()).toStream.flatMap { h =>
      blockAt(h).flatMap { block =>
        if (block.consensusModule.generators(block).contains(account)) Some(block) else None
      }
    }

  override def toString: String = ((1 to height()) map { case h =>
    val bl = blockAt(h).get
    s"$h -- ${bl.uniqueId.mkString} -- ${bl.referenceField.value.mkString}"
  }).mkString("\n")
}
