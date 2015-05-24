package scorex.database.blockchain

import java.io.DataInputStream

import com.google.common.primitives.Ints
import org.mapdb.DBMaker
import scorex.account.Account
import scorex.block.Block
import settings.Settings

import scala.collection.JavaConversions._
import scala.reflect.io.File
import scala.util.Try

class BlockchainImpl extends BlockChain {
  private val database = DBMaker.newFileDB(new java.io.File(Settings.dataDir + s"/signatures"))
    .closeOnJvmShutdown()
    .checksumEnable()
    .mmapFileEnableIfSupported()
    .make()

  private val signaturesIndex = database.createTreeMap("signatures").makeOrGet[Int, Array[Byte]]()

  //if there are some uncommited changes from last run, discard'em
  if (signaturesIndex.size() > 0) database.rollback()

  private def blockFile(height: Int) = File(Settings.dataDir + s"/block-$height")

  override def appendBlock(block: Block): BlockChain = synchronized {
    val h = height() + 1
    signaturesIndex.put(h, block.signature)

    val blockBytes = block.toBytes
    val os = blockFile(h).outputStream(append = false)
    try {
      os.write(Ints.toByteArray(blockBytes.length))
      os.write(blockBytes)
      os.flush()
    } finally os.close()

    database.commit()
    this
  }

  override def discardBlock(): BlockChain = synchronized {
    require(height() > 1, "Chain is empty or contains genesis block only, can't make rollback")
    val h = height()
    Try(blockFile(h).delete()) //todo: write msg to log if problems with deleting
    signaturesIndex.remove(h)
    database.commit()
    this
  }

  override def heightOf(block: Block): Option[Int] =
    signaturesIndex.descendingMap().find(_._2.sameElements(block.signature)).map(_._1)

  override def blockAt(height: Int): Option[Block] = synchronized {
    val is = new DataInputStream(blockFile(height).inputStream())
    try {
      val szBytes = new Array[Byte](4)
      is.read(szBytes, 0, 4)
      val sz = Ints.fromByteArray(szBytes)
      val bytes = new Array[Byte](sz)
      is.read(bytes)
      Block.parse(bytes).toOption
    } finally is.close()
  }

  override def contains(block: Block): Boolean = contains(block.signature)

  override def contains(signature: Array[Byte]): Boolean = signaturesIndex.exists(_._2.sameElements(signature))

  override def height(): Int = Option(signaturesIndex.size).getOrElse(0)

  override def heightOf(blockSignature: Array[Byte]): Option[Int] =
    signaturesIndex.find(_._2.sameElements(blockSignature)).map(_._1)

  override def blockByHeader(signature: Array[Byte]): Option[Block] =
    heightOf(signature).flatMap(blockAt)

  //todo: implement
  override def child(block: Block): Option[Block] = ???

  //todo: implement
  override def generatedBy(account: Account): Seq[Block] = ???
}