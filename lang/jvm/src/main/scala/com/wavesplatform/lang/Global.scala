package com.wavesplatform.lang

import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.v1.BaseGlobal
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash._
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.util.Try

object Global extends BaseGlobal {
  def base58Encode(input: Array[Byte]): Either[String, String] =
    if (input.length > MaxBase58Bytes) Left(s"base58Encode input exceeds $MaxBase58Bytes")
    else Right(Base58.encode(input))

  def base58Decode(input: String, limit: Int): Either[String, Array[Byte]] =
    if (input.length > limit) Left(s"base58Decode input exceeds $limit")
    else Base58.tryDecodeWithLimit(input, limit).toEither.left.map(_ => "can't parse Base58 string")

  def base64Encode(input: Array[Byte]): Either[String, String] =
    Either.cond(input.length <= MaxBase64Bytes, Base64.encode(input), s"base64Encode input exceeds $MaxBase64Bytes")

  def base64Decode(input: String, limit: Int): Either[String, Array[Byte]] =
    for {
      _      <- Either.cond(input.length <= limit, (), s"base64Decode input exceeds $limit")
      result <- Base64.tryDecode(input).toEither.left.map(_ => "can't parse Base64 string")
    } yield result

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = Curve25519.verify(Signature(sig), message, PublicKey(pub))

  def keccak256(message: Array[Byte]): Array[Byte]  = Keccak256.hash(message)
  def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)
  def sha256(message: Array[Byte]): Array[Byte]     = Sha256.hash(message)

  override def merkleVerify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean = {
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
