package com.wavesplatform.common.merkle

import com.wavesplatform.common.merkle.ScryptoMerkleProof.*

import scala.util.Try

case class ScryptoMerkleProof(leafData: LeafData, levels: Seq[(Digest, Byte)]) {
  def valid(expectedRootHash: Digest): Boolean = {
    val leafHash = Merkle.hash(LeafPrefix +: leafData)

    levels
      .foldLeft(leafHash) { case (prevHash, (hash, side)) =>
        if (side == ScryptoMerkleProof.LeftSide) {
          Merkle.hash(InternalNodePrefix +: (prevHash ++ hash))
        } else {
          Merkle.hash(InternalNodePrefix +: (hash ++ prevHash))
        }
      }
      .sameElements(expectedRootHash)
  }
}

object ScryptoMerkleProof {
  val LeftSide: Byte  = 0.toByte
  val RightSide: Byte = 1.toByte

  val LeafPrefix: Byte         = 0: Byte
  val InternalNodePrefix: Byte = 1: Byte

  def parse(proofBytes: Array[Byte], valueHash: Array[Byte]): Option[ScryptoMerkleProof] =
    parseMerkleProofLevels(proofBytes).map { data =>
      ScryptoMerkleProof(valueHash, data)
    }.toOption

  private def parseMerkleProofLevels(arr: Array[Byte]): Either[String, List[(Digest, Byte)]] = {
    def parseHashAndSide(arr: Array[Byte]): Either[String, (Byte, Digest, Array[Byte])] = {
      val side =
        if (arr(0) == ScryptoMerkleProof.LeftSide) ScryptoMerkleProof.LeftSide
        else ScryptoMerkleProof.RightSide
      val hashLen   = arr(1).toInt
      lazy val hash = arr.slice(2, 2 + hashLen)

      Either
        .cond(
          hashLen >= 0,
          (side, hash, arr.drop(2 + hashLen)),
          s"Invalid proof hash length: $hashLen"
        )
    }

    def parseLevels(arr: Array[Byte], acc: List[(Digest, Byte)]): Either[String, List[(Digest, Byte)]] = {
      if (arr.nonEmpty) {
        parseHashAndSide(arr)
          .flatMap { case (side, hash, rest) =>
            parseLevels(rest, (hash, side) :: acc)
          }
      } else Right(acc.reverse)
    }

    Try(parseLevels(arr, Nil))
      .getOrElse(Left("Can't parse proof bytes"))
  }
}
