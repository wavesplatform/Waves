package scorex.transaction.state.database.blockchain

import java.io.DataInputStream

import com.google.common.primitives.Ints
import org.mapdb.DBMaker
import scorex.account.Account
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.transaction.{TransactionModule, BlockChain}
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scala.reflect.io.File
import scala.util.Try

/**
 * If no datafolder provided, blockchain lives in RAM (useful for tests)
 */

class StoredBlockchain(dataFolderOpt: Option[String])
                      (implicit consensusModule: ConsensusModule[_],
                       transactionModule: TransactionModule[_])
  extends BlockChain with ScorexLogging {

  trait BlockStorage {
    def writeBlock(height: Int, block: Block): Unit

    def readBlock(height: Int): Option[Block]

    def deleteBlock(height: Int): Unit
  }

  case class FileBlockStorage(dataFolder: String) extends BlockStorage {
    private def blockFile(height: Int) = File(dataFolder + s"/block-$height")

    //todo: return Try[Unit] instead of Unit?
    override def writeBlock(height: Int, block: Block): Unit = {
      val blockBytes = block.bytes
      val os = blockFile(height).outputStream(append = false)
      try {
        os.write(Ints.toByteArray(blockBytes.length))
        os.write(blockBytes)
        os.flush()
      } finally os.close()
    }

    //todo: exceptions handling, err log output
    override def readBlock(height: Int): Option[Block] = {
      val file = blockFile(height)

      val is = new DataInputStream(file.inputStream())
      try {
        val szBytes = new Array[Byte](4)
        is.read(szBytes, 0, 4)
        val sz = Ints.fromByteArray(szBytes)
        val bytes = new Array[Byte](sz)
        is.read(bytes)
        Block.parse(bytes).toOption
      } finally is.close()
    }

    override def deleteBlock(height: Int): Unit =
      Try(blockFile(height).delete()).recover { case t =>
        log.error(s"Can't delete blockfile: ${blockFile(height).name}", t)
      }
  }

  object MemoryBlockStorage extends BlockStorage {
    private val memStorage = TrieMap[Int, Block]()

    override def writeBlock(height: Int, block: Block): Unit = memStorage.put(height, block)

    override def readBlock(height: Int): Option[Block] = memStorage.get(height)

    override def deleteBlock(height: Int): Unit = memStorage.remove(height)
  }

  private val (blockStorage, database) = dataFolderOpt match {
    case Some(dataFolder) =>
      (FileBlockStorage(dataFolder),
        DBMaker.appendFileDB(new java.io.File(dataFolder + s"/signatures"))
          .fileMmapEnableIfSupported()
          .closeOnJvmShutdown()
          .checksumEnable()
          .make())
    case None =>
      (MemoryBlockStorage, DBMaker.memoryDB().make())
  }

  private val signaturesIndex = database.createTreeMap("signatures").makeOrGet[Int, Array[Byte]]()

  //if there are some uncommited changes from last run, discard'em
  if (signaturesIndex.size() > 0) database.rollback()

  override def appendBlock(block: Block): BlockChain = synchronized {
    val h = height() + 1
    signaturesIndex.put(h, block.uniqueId)
    blockStorage.writeBlock(h, block)
    database.commit()
    this
  }

  override def discardBlock(): BlockChain = synchronized {
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

  override def contains(block: Block): Boolean = contains(block.uniqueId)

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

  override def toString() = ((1 to height()) map {case h =>
      val bl = blockAt(h).get
      s"$h -- ${bl.uniqueId.mkString} -- ${bl.referenceField.value.mkString}"
  }).mkString("\n")
}