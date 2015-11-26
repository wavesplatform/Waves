package scorex.crypto.ads.merkle

import play.api.libs.json._
import scorex.crypto.CryptographicHash._
import scorex.crypto.ads.merkle.TreeStorage.Position
import scorex.crypto.{CryptographicHash, Sha256}

import scala.annotation.tailrec

/**
  * @param data - data block
  * @param merklePath - merkle path, complementary to data block
  */
case class AuthDataBlock[Block](data: Block, merklePath: Seq[Digest]) {

  def check[Hash <: CryptographicHash](index: Position, rootHash: Digest)
                                      (hashFunction: Hash = Sha256): Boolean = {

    @tailrec
    def calculateHash(i: Position, nodeHash: Digest, path: Seq[Digest]): Digest = {
      val hash = if (i % 2 == 0)
        hashFunction.hash(nodeHash ++ path.head)
      else
        hashFunction.hash(path.head ++ nodeHash)

      if (path.size == 1)
        hash
      else
        calculateHash(i / 2, hash, path.tail)
    }

    if (merklePath.nonEmpty)
      calculateHash(index, hashFunction.hash(data.asInstanceOf[Message]), merklePath) sameElements rootHash
    else
      false
  }
}

object AuthDataBlock {

  implicit def authDataBlockReads[T](implicit fmt: Reads[T]): Reads[AuthDataBlock[T]] = new Reads[AuthDataBlock[T]] {
    def reads(json: JsValue): JsResult[AuthDataBlock[T]] = JsSuccess(AuthDataBlock[T](
      (json \ "data").get match {
        case JsString(ts) => ts.getBytes.asInstanceOf[T]
        case _ => throw new RuntimeException("Data MUST be a string")
      },
      (json \ "merklePath").get match {
        case JsArray(ts) => ts.map(t => t.as[String].getBytes)
        case _ => throw new RuntimeException("MerklePath MUST be a list")
      }
    ))
  }

  implicit def authDataBlockWrites[T](implicit fmt: Writes[T]): Writes[AuthDataBlock[T]] = new Writes[AuthDataBlock[T]] {
    def writes(ts: AuthDataBlock[T]) = JsObject(Seq(
      "data" -> JsString(new String(ts.data.asInstanceOf[Array[Byte]])),
      "merklePath" -> JsArray(
        ts.merklePath.map(digest => JsString(new String(digest)))
      )
    ))
  }

}

