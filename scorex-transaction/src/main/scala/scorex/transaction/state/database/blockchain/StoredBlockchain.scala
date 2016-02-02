package scorex.transaction.state.database.blockchain

import org.mapdb.{DB, DBMaker}
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.ConsensusModule
import scorex.transaction.BlockStorage._
import scorex.transaction.{BlockChain, TransactionModule}
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

/**
  * If no datafolder provided, blockchain lives in RAM (useful for tests)
  */
class StoredBlockchain(dataFolderOpt: Option[String])
                      (implicit consensusModule: ConsensusModule[_],
                       transactionModule: TransactionModule[_])
  extends BlockChain with ScorexLogging {

  case class BlockchainPersistence(database: DB) {
    val blocks = database.treeMap[Int, Array[Byte]]("blocks")
    val signatures = database.treeMap[Int, BlockId]("signatures")

    //if there are some uncommited changes from last run, discard'em
    if (signatures.size() > 0) database.rollback()

    def writeBlock(height: Int, block: Block): Try[Unit] = Try {
      blocks.put(height, block.bytes)
      signatures.put(height, block.uniqueId)
      database.commit()
    }

    def readBlock(height: Int): Option[Block] =
      Try(Option(blocks.get(height))).toOption.flatten.flatMap(b => Block.parse(b).toOption)

    def deleteBlock(height: Int): Unit = {
      blocks.remove(height)
      signatures.remove(height)
      database.commit()
    }

    def contains(id: BlockId): Boolean = signatures.exists(_._2.sameElements(id))

    def height(): Int = signatures.size()

    def heightOf(id: BlockId): Option[Int] = signatures.find(_._2.sameElements(id)).map(_._1)

  }

  private val blockStorage: BlockchainPersistence = {
    val db = dataFolderOpt match {
      case Some(dataFolder) =>
        DBMaker.appendFileDB(new java.io.File(dataFolder + s"/blocks"))
          .fileMmapEnableIfSupported()
          .closeOnJvmShutdown()
          .checksumEnable()
          .make()
      case None => DBMaker.memoryDB().make()
    }
    new BlockchainPersistence(db)
  }


  log.info(s"Initialized blockchain in $dataFolderOpt with ${height()} blocks")

  override private[transaction] def appendBlock(block: Block): Try[BlocksToProcess] = synchronized {
    Try {
      val parent = block.referenceField
      if ((height() == 0) || (lastBlock.uniqueId sameElements parent.value)) {
        val h = height() + 1
        blockStorage.writeBlock(h, block) match {
          case Success(_) => Seq(block)
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
              (b, Reversed)
            }
            val h = height() + 1
            blockStorage.writeBlock(h, block) match {
              case Success(_) => Seq(block)
              case Failure(t) => throw new Error("Error while storing blockchain a change: " + t)
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
    this
  }

  override def blockAt(height: Int): Option[Block] = synchronized {
    blockStorage.readBlock(height)
  }

  override def contains(signature: Array[Byte]): Boolean = blockStorage.contains(signature)

  override def height(): Int = blockStorage.height()

  override def heightOf(blockSignature: Array[Byte]): Option[Int] = blockStorage.heightOf(blockSignature)

  override def blockById(blockId: BlockId): Option[Block] = heightOf(blockId).flatMap(blockAt)

  override def children(block: Block): Seq[Block] = heightOf(block).flatMap(h => blockAt(h + 1)).toSeq

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
