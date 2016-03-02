package scorex.crypto.ads.merkle

import com.google.common.primitives.{Bytes, Ints}
import play.api.libs.json._
import scorex.crypto.ads.merkle.TreeStorage.Position
import scorex.crypto.encode.Base58
import scorex.crypto.hash.CryptographicHash._
import scorex.crypto.hash.{CryptographicHash, FastCryptographicHash}

import scala.annotation.tailrec
import scala.util.Try

/**
  * @param data - data block
  * @param merklePath - merkle path, complementary to data block
  */
case class AuthDataBlock[Block](data: Block, merklePath: Seq[Digest]) {

  /**
    * Checks that this block is at position $index in tree with root hash = $rootHash
    */
  def check[HashImpl <: CryptographicHash](index: Position, rootHash: Digest)
                                          (hashFunction: HashImpl = FastCryptographicHash): Boolean = {

    @tailrec
    def calculateHash(idx: Position, nodeHash: Digest, path: Seq[Digest]): Digest = {
      val hash = if (idx % 2 == 0)
        hashFunction(nodeHash ++ path.head)
      else
        hashFunction(path.head ++ nodeHash)

      if (path.size == 1)
        hash
      else
        calculateHash(idx / 2, hash, path.tail)
    }

    if (merklePath.nonEmpty)
      calculateHash(index, hashFunction(data.asInstanceOf[Message]), merklePath) sameElements rootHash
    else
      false
  }
}

object AuthDataBlock {

  def encode(b: AuthDataBlock[Array[Byte]]): Array[Byte] = {
    require(b.merklePath.nonEmpty, "Merkle path cannot be empty")
    val dataSize = Bytes.ensureCapacity(Ints.toByteArray(b.data.length), 4, 0)
    val merklePathLength = Bytes.ensureCapacity(Ints.toByteArray(b.merklePath.length), 4, 0)
    val merklePathSize = Bytes.ensureCapacity(Ints.toByteArray(b.merklePath.head.length), 4, 0)
    val merklePath = b.merklePath.foldLeft(Array.empty: Array[Byte])((b, mp) => b ++ mp)
    dataSize ++ merklePathLength ++ merklePathSize ++ b.data ++ merklePath
  }

  def decode(bytes: Array[Byte]): Try[AuthDataBlock[Array[Byte]]] = Try {
    val dataSize = Ints.fromByteArray(bytes.slice(0, 4))
    val merklePathLength = Ints.fromByteArray(bytes.slice(4, 8))
    val merklePathSize = Ints.fromByteArray(bytes.slice(8, 12))
    val data = bytes.slice(12, 12 + dataSize)
    val merklePathStart = 12 + dataSize
    val merklePath = (0 until merklePathLength).map { i =>
      bytes.slice(merklePathStart + i * merklePathSize, merklePathStart + (i + 1) * merklePathSize)
    }
    AuthDataBlock(data, merklePath)
  }

  implicit def authDataBlockReads[T](implicit fmt: Reads[T]): Reads[AuthDataBlock[T]] = new Reads[AuthDataBlock[T]] {
    def reads(json: JsValue): JsResult[AuthDataBlock[T]] = JsSuccess(AuthDataBlock[T](
      (json \ "data").get match {
        case JsString(ts) =>
          Base58.decode(ts).get.asInstanceOf[T]
        case _ =>
          throw new RuntimeException("Data MUST be a string")
      },
      (json \ "merklePath").get match {
        case JsArray(ts) => ts.map { t =>
          t match {
            case JsString(digest) =>
              Base58.decode(digest)
            case m =>
              throw new RuntimeException("MerklePath MUST be array of strings" + m + " given")
          }
        }.map(_.get)
        case m =>
          throw new RuntimeException("MerklePath MUST be a list " + m + " given")
      }
    ))
  }

  implicit def authDataBlockWrites[T](implicit fmt: Writes[T]): Writes[AuthDataBlock[T]] = new Writes[AuthDataBlock[T]] {
    def writes(ts: AuthDataBlock[T]) = JsObject(Seq(
      "data" -> JsString(Base58.encode(ts.data.asInstanceOf[Array[Byte]])),
      "merklePath" -> JsArray(
        ts.merklePath.map(digest => JsString(Base58.encode(digest)))
      )
    ))
  }
}

