package scorex.crypto.ads.merkle

import java.io.{FileOutputStream, RandomAccessFile}
import java.nio.file.{Files, Paths}

import scorex.crypto.ads.merkle.TreeStorage.Position
import scorex.crypto.hash.CryptographicHash.Digest
import scorex.crypto.hash.{CryptographicHash, FastCryptographicHash}
import scorex.utils.ScorexLogging

import scala.annotation.tailrec

class MerkleTree[H <: CryptographicHash](treeFolder: String,
                                         val nonEmptyBlocks: Position,
                                         blockSize: Int = 1024,
                                         hash: H = FastCryptographicHash
                                        ) extends ScorexLogging {

  import MerkleTree._

  val level = calculateRequiredLevel(nonEmptyBlocks)

  lazy val storage = new TreeStorage(treeFolder + TreeFileName, level)

  val rootHash: Digest = getHash((level, 0)).get

  storage.commit()

  def byIndex(index: Position): Option[AuthDataBlock[Block]] = {
    if (index < nonEmptyBlocks && index >= 0) {
      @tailrec
      def calculateTreePath(n: Position, currentLevel: Int, acc: Seq[Digest] = Seq()): Seq[Digest] = {
        if (currentLevel < level) {
          val hashOpt = if (n % 2 == 0) getHash((currentLevel, n + 1)) else getHash((currentLevel, n - 1))
          hashOpt match {
            case Some(h) =>
              calculateTreePath(n / 2, currentLevel + 1, h +: acc)
            case None if currentLevel == 0 && index == nonEmptyBlocks - 1 =>
              calculateTreePath(n / 2, currentLevel + 1, emptyHash +: acc)
            case None =>
              log.error(s"Enable to get hash for lev=$currentLevel, position=$n")
              acc.reverse
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

  private lazy val emptyHash = hash("")

  def getHash(key: TreeStorage.Key): Option[Digest] = {
    storage.get(key) match {
      case None =>
        if (key._1 > 0) {
          val h1 = getHash((key._1 - 1, key._2 * 2))
          val h2 = getHash((key._1 - 1, key._2 * 2 + 1))
          val calculatedHash = (h1, h2) match {
            case (Some(hash1), Some(hash2)) => hash(hash1 ++ hash2)
            case (Some(h), _) => hash(h ++ emptyHash)
            case (_, Some(h)) => hash(emptyHash ++ h)
            case _ => emptyHash
          }
          storage.set(key, calculatedHash)
          Some(calculatedHash)
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

  val TreeFileName = "/hashTree"

  def fromFile[H <: CryptographicHash](fileName: String,
                                       treeFolder: String,
                                       blockSize: Int = 1024,
                                       hash: H = FastCryptographicHash
                                      ): MerkleTree[H] = {
    val byteBuffer = new Array[Byte](blockSize)

    def readLines(bigDataFilePath: String, chunkIndex: Position): Array[Byte] = {
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

    val nonEmptyBlocks: Position = {
      val randomAccessFile = new RandomAccessFile(fileName, "r")
      try {
        (randomAccessFile.length / blockSize).toInt
      } finally {
        randomAccessFile.close()
      }
    }

    val level = calculateRequiredLevel(nonEmptyBlocks)

    lazy val storage = new TreeStorage(treeFolder + TreeFileName, level)

    def processBlocks(currentBlock: Position = 0): Unit = {
      val block: Block = readLines(fileName, currentBlock)
      val fos = new FileOutputStream(treeFolder + "/" + currentBlock)
      fos.write(block)
      fos.close()
      storage.set((0, currentBlock), hash(block))
      if (currentBlock < nonEmptyBlocks - 1) {
        processBlocks(currentBlock + 1)
      }
    }

    processBlocks()

    storage.commit()
    storage.close()

    new MerkleTree(treeFolder, nonEmptyBlocks, blockSize, hash)
  }

  private def log2(x: Double): Double = math.log(x) / math.log(2)

  def calculateRequiredLevel(numberOfDataBlocks: Position): Int = {
    math.ceil(log2(numberOfDataBlocks)).toInt
  }
}