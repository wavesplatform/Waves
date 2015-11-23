package scorex.crypto.ads.merkle

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

    if (merklePath.nonEmpty) {
      val calculated = calculateHash(index, hashFunction.hash(data.asInstanceOf[Message]), merklePath)
      calculated.mkString == rootHash.mkString
    } else true
  }
}
