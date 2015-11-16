package scorex.crypto.ads.merkle

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}

import scorex.crypto.CryptographicHash.Digest
import scorex.crypto.{CryptographicHash, Sha256}

import scala.annotation.tailrec

class MerkleTree[H <: CryptographicHash](treeFolder: String,
                                         val nonEmptyBlocks: Int,
                                         blockSize: Int = 1024,
                                         hash: H = Sha256
                                        ) {

  import MerkleTree._

  lazy val storage: Storage = new MapDBStorage(new File(treeFolder + "/tree.mapDB"))

  val level = calculateRequiredLevel(nonEmptyBlocks)

  lazy val rootHash: Digest = getHash((level, 0)).get

  def byIndex(index: Int): Option[AuthDataBlock[Block]] = {
    if (index < nonEmptyBlocks && index >= 0) {
      @tailrec
      def calculateTreePath(n: Int, currentLevel: Int, acc: Seq[Digest] = Seq()): Seq[Digest] = {
        if (currentLevel < level) {
          //TODO remove get? it should exists when (index < nonEmptyBlocks && index > 0)
          if (n % 2 == 0) {
            getHash((currentLevel, n + 1)) match {
              case Some(v) =>
                calculateTreePath(n / 2, currentLevel + 1, v +: acc)
              case None =>
                acc.reverse
            }
          } else {
            calculateTreePath(n / 2, currentLevel + 1, getHash((currentLevel, n - 1)).get +: acc)
          }
        } else {
          acc.reverse
        }
      }

      val path = Paths.get(treeFolder + "/" + index)
      val data: Block = Files.readAllBytes(path)
      val treePath = calculateTreePath(index, 0)
      Some(AuthDataBlock(data, treePath))
    } else {
      None
    }
  }

  def getHash(key: Storage.Key): Option[Digest] = {
    storage.get(key) match {
      case None =>
        if (key._1 > 0) {
          val h1 = getHash((key._1 - 1, key._2 * 2))
          val h2 = getHash((key._1 - 1, key._2 * 2 + 1))
          (h1, h2) match {
            case (Some(hash1), Some(hash2)) => Some(hash.hash(hash1 ++ hash2))
            case (Some(h), _) => h1
            case (_, Some(h)) => h2
            case _ => None
          }
        } else {
          None
        }
      case digest =>
        digest
    }

  }


}

object MerkleTree {
  type Block = Array[Byte]


  def fromFile[H <: CryptographicHash](file: FileInputStream,
                                       treeFolder: String,
                                       blockSize: Int = 1024,
                                       hash: H = Sha256
                                      ): MerkleTree[H] = {

    @tailrec
    def processFile(file: FileInputStream, blockIndex: Int = 0): Int = {
      val buf = new Array[Byte](blockSize)
      val length = file.read(buf)
      if (length != -1) {
        file.read(buf, 0, length)
        processBlock(buf, blockIndex)
        processFile(file, blockIndex + 1)
      } else {
        blockIndex
      }
    }

    def processBlock(block: Block, i: Int): Unit = {
      val fos = new FileOutputStream(treeFolder + "/" + i)
      fos.write(block)
      fos.close()
      storage.set((0, i), hash.hash(block))
    }

    lazy val storage: Storage = new MapDBStorage(new File(treeFolder + "/tree.mapDB"))

    val nonEmptyBlocks = processFile(file)
    storage.commit()

    new MerkleTree(treeFolder, nonEmptyBlocks, blockSize, hash)

  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  def calculateRequiredLevel(numberOfDataBlocks: Int): Int = {

    math.ceil(log2(numberOfDataBlocks)).toInt
  }


}
