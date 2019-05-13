package com.wavesplatform.lang

import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.v1.BaseGlobal
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

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

  val hex : Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
  def base16Encode(input: Array[Byte]): Either[String, String] = {
    val output = new StringBuilder(input.size * 2)
    for (b <- input) {
       output.append(hex(b >> 4))
       output.append(hex(b & 0xf))
    }
    Right(output.result)
  }

  def base16Dig(c: Char): Either[String, Byte] = {
    if ('0' <= c && c <= '9') {
      Right((c - '0').toByte)
    } else if ('a' <= c && c <= 'f') {
      Right((10 + (c - 'a')).toByte)
    } else if ('A' <= c && c <= 'F') {
      Right((10 + (c - 'A')).toByte)
    } else {
      Left(s"$c isn't base16/hex digit")
    }
  }

  def base16Decode(input: String, limit: Int): Either[String, Array[Byte]] = {
    val size = input.size
    if(size % 2 == 1) {
      Left("Need integnal bytes number")
    } else {
      val bytes = new Array[Byte](size / 2)
      for( i <- 0 to size ) {
        (base16Dig(input(i*2)), base16Dig(input(i*2 + 1))) match {
          case (Right(h), Right(l)) => bytes(i) = ((16:Byte)*h + l).toByte
          case (Left(e),_) => return Left(e)
          case (_,Left(e)) => return Left(e)
        }
      }
      Right(bytes)
    }
  }

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = Curve25519.verify(Signature(sig), message, PublicKey(pub))

  def keccak256(message: Array[Byte]): Array[Byte]  = Keccak256.hash(message)
  def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)
  def sha256(message: Array[Byte]): Array[Byte]     = Sha256.hash(message)
}
