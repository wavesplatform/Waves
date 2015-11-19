package scorex.crypto.ads.merkle

import scorex.crypto.ads.merkle.Storage.Position
import scorex.crypto.{Sha256, CryptographicHash}
import scorex.crypto.CryptographicHash._

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
      if (i % 2 == 0) {
        val hash = hashFunction.hash(nodeHash ++ path.head)
        if (path.size == 1) {
          hash
        } else {
          calculateHash(i / 2, hash, path.tail)
        }
      } else {
        val hash = hashFunction.hash(path.head ++ nodeHash)
        if (path.size == 1) {
          hash
        } else {
          calculateHash(i / 2, hash, path.tail)
        }
      }
    }
    if (merklePath.nonEmpty) {
      val calculated = calculateHash(index, hashFunction.hash(data.asInstanceOf[Message]), merklePath)
      calculated.mkString == rootHash.mkString
    } else {
      true
    }
  }
}

