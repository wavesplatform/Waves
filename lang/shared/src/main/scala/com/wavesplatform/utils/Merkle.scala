package com.wavesplatform.utils

import com.wavesplatform.lang.v1.BaseGlobal
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.authds.{LeafData, Side}
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
    parseMerkleProofLevels(proofBytes).map { data =>
      MerkleProof[Digest32](
        LeafData @@ valueBytes,
        data
      )(blakeCH)
    }.toOption

  def parseMerkleProofLevels(arr: Array[Byte]): Either[String, List[(Digest, Side)]] = {
    def parseHashAndSide(arr: Array[Byte]): Either[String, (Side, Digest, Array[Byte])] = {
      val side =
        if (arr(0) == MerkleProof.LeftSide) MerkleProof.LeftSide
        else MerkleProof.RightSide
      val hashLen   = arr(1).toInt
      lazy val hash = Digest32 @@ arr.slice(2, 2 + hashLen)

      Either
        .cond(
          hashLen >= 0,
          (side, hash, arr.drop(2 + hashLen)),
          s"Invalid proof hash length: $hashLen"
        )
    }

    def parseLevels(arr: Array[Byte], acc: List[(Digest, Side)]): Either[String, List[(Digest, Side)]] = {
      if (arr.nonEmpty) {
        parseHashAndSide(arr)
          .flatMap {
            case (side, hash, rest) =>
              parseLevels(rest, (hash, side) :: acc)
          }
      } else Right(acc.reverse)
    }

    Try(parseLevels(arr, Nil))
      .getOrElse(Left("Can't parse proof bytes"))
  }

  private val global: BaseGlobal = com.wavesplatform.lang.Global
  private val blakeCH: CryptographicHash[Digest32] =
    new CryptographicHash32 {
      override def hash(input: Message): Digest32 = Digest32 @@ global.blake2b256(input)
    }
}
