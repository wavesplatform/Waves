package com.wavesplatform.utils

import com.wavesplatform.lang.Global.blake2b256
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.{CryptographicHash, CryptographicHash32, Digest, Digest32}

import scala.util.Try

object Merkle {

  def verify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean = {
    (for {
      rootDigest  <- parseRoot(rootBytes)
      merkleProof <- parseProof(proofBytes, valueBytes)
    } yield merkleProof.valid(rootDigest)).getOrElse(false)
  }

  def parseRoot(bytes: Array[Byte]): Option[Digest32] = {
    if (bytes.length == 32) Some(Digest32 @@ bytes)
    else None
  }

  private def parseProof(proofBytes: Array[Byte], valueBytes: Array[Byte]): Option[MerkleProof[Digest32]] =
    Try {
      MerkleProof[Digest32](
        LeafData @@ valueBytes,
        parseMerkleProofLevels(proofBytes)
      )(blakeCH)
    }.toOption

  def parseMerkleProofLevels(arr: Array[Byte]): List[(Digest, Side)] = {
    def parseHashAndSide(arr: Array[Byte]): (Side, Digest, Array[Byte]) = {
      val side =
        if (arr(0) == MerkleProof.LeftSide) MerkleProof.LeftSide
        else MerkleProof.RightSide
      val hashLen = arr(1).toInt
      val hash    = Digest32 @@ arr.slice(2, 2 + hashLen)
      (side, hash, arr.drop(2 + hashLen))
    }

    def parseLevels(arr: Array[Byte], acc: List[(Digest, Side)]): List[(Digest, Side)] = {
      if (arr.nonEmpty) {
        val (side, hash, rest) = parseHashAndSide(arr)
        parseLevels(rest, (hash, side) :: acc)
      } else acc.reverse
    }

    parseLevels(arr, Nil)
  }

  private val blakeCH: CryptographicHash[Digest32] =
    new CryptographicHash32 {
      override def hash(input: Message): Digest32 = Digest32 @@ blake2b256(input)
    }
}
