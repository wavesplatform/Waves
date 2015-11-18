package scorex.crypto.ads.merkle

import java.io.{File, FileOutputStream, RandomAccessFile}
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
            calculateTreePath(n / 2, currentLevel + 1, getHash((currentLevel, n + 1)).get +: acc)
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

  private lazy val emptyHash = hash.hash("".getBytes)

  def getHash(key: Storage.Key): Option[Digest] = {
    storage.get(key) match {
      case None =>
        if (key._1 > 0) {
          val h1 = getHash((key._1 - 1, key._2 * 2))
          val h2 = getHash((key._1 - 1, key._2 * 2 + 1))
          (h1, h2) match {
            case (Some(hash1), Some(hash2)) => Some(hash.hash(hash1 ++ hash2))
            case (Some(h), _) => Some(hash.hash(h ++ emptyHash))
            case (_, Some(h)) => Some(hash.hash(emptyHash ++ h))
            case _ => Some(emptyHash)
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


  def fromFile[H <: CryptographicHash](fileName: String,
                                       treeFolder: String,
                                       blockSize: Int = 1024,
                                       hash: H = Sha256
                                      ): MerkleTree[H] = {
    lazy val storage: Storage = new MapDBStorage(new File(treeFolder + "/tree.mapDB"))

    def processBlock(block: Block, i: Int): Unit = {
      val fos = new FileOutputStream(treeFolder + "/" + i)
      fos.write(block)
      fos.close()
      storage.set((0, i), hash.hash(block))
    }

    val byteBuffer = new Array[Byte](blockSize)

    def readLines(bigDataFilePath: String, chunkIndex: Int): Array[Byte] = {
      val randomAccessFile = new RandomAccessFile(fileName, "r")
      try {
        val seek = chunkIndex * blockSize
        randomAccessFile.seek(seek)
        randomAccessFile.read(byteBuffer)
        byteBuffer
      } finally {
        randomAccessFile.close()
      }
    }

    val nonEmptyBlocks = {
      val randomAccessFile = new RandomAccessFile(fileName, "r")
      try {
        (randomAccessFile.length / blockSize).toInt
      } finally {
        randomAccessFile.close()
      }
    }

    for (i <- 0 to (nonEmptyBlocks - 1)) {
      val block = readLines(fileName, i)
      processBlock(block, i)
    }


    storage.commit()

    new MerkleTree(treeFolder, nonEmptyBlocks, blockSize, hash)

  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  def calculateRequiredLevel(numberOfDataBlocks: Int): Int = {

    math.ceil(log2(numberOfDataBlocks)).toInt
  }


}
