package com.wavesplatform.common.merkle

import scala.annotation.tailrec

object Merkle {
  private val empty: Digest           = hash(Array[Byte](0))
  private val emptyLevels: Seq[Level] = Seq(Seq(empty))

  @inline private def isLeft(i: Int): Boolean = i % 2 == 0

  /** Hash function */
  def hash(input: Message): Digest = com.wavesplatform.lang.Global.blake2b256(input)

  /** Makes levels of merkle tree (from top to bottom) */
  def mkLevels(data: Seq[Message]): Seq[Level] = {
    if (data.isEmpty) emptyLevels
    else {
      @tailrec
      def loop(prevLevel: Seq[Digest], acc: Seq[Level]): Seq[Level] = {
        val level = prevLevel
          .grouped(2)
          .collect {
            case Seq(l, r) => hash(l ++ r)
            case Seq(l)    => hash(l ++ empty)
          }
          .toSeq
        if (level.size == 1) level +: acc else loop(level, level +: acc)
      }
      val bottom = data.map(hash)
      loop(bottom, Seq(bottom))
    }
  }

  /** Makes proofs for data (from top to bottom)
    *
    * @param index
    *   of data
    * @param levels
    *   of merkle tree (from top to bottom)
    */
  def mkProofs(index: Int, levels: Seq[Level]): Seq[Digest] = {
    val (result, _) = levels.tail.reverse.foldLeft((Seq.empty[Digest], index)) { case ((proofs, idx), level) =>
      val proof = isLeft(idx) match {
        case true if idx + 1 == level.size => empty
        case true                          => level(idx + 1)
        case false                         => level(idx - 1)
      }
      (proof +: proofs, idx / 2)
    }
    result
  }

  def createRoot(digest: Digest, index: Int, proofs: Seq[Digest]): Digest = {
    val (calculated, rest) = proofs.reverse.foldLeft((digest, index)) {
      case ((left, idx), right) if isLeft(idx) => (hash(left ++ right), idx / 2)
      case ((right, idx), left)                => (hash(left ++ right), idx / 2)
    }
    if (rest != 0) {
      throw new Exception(s"Index $index out of range allowed by proof list length ${proofs.length}")
    }
    calculated
  }

  /** Verifies proofs
    *
    * @param digest
    *   data digest
    * @param index
    *   data index
    * @param proofs
    *   merkle proofs (from top to bottom)
    * @param root
    *   merkle root
    */
  def verify(digest: Digest, index: Int, proofs: Seq[Digest], root: Digest): Boolean = {
    (1 << proofs.length) > index && index >= 0 && (createRoot(digest, index, proofs) sameElements root)
  }

  def verify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean = {
    (for {
      rootDigest  <- parseRoot(rootBytes)
      merkleProof <- ScryptoMerkleProof.parse(proofBytes, valueBytes)
    } yield merkleProof.valid(rootDigest)).getOrElse(false)
  }

  def parseRoot(bytes: Array[Byte]): Option[Digest] = if (bytes.length == 32) Some(bytes) else None
}
