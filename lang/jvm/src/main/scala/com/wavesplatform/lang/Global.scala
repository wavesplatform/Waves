package com.wavesplatform.lang

import java.math.{MathContext, BigDecimal => BD}

import ch.obermuhlner.math.big.BigDecimalMath
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse
import com.wavesplatform.utils.Merkle
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.concurrent.Future
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

  override def rsaVerify(alg: DigestAlgorithm, message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = RSA.verify(alg, message, sig, pub)

  def keccak256(message: Array[Byte]): Array[Byte]  = Keccak256.hash(message)
  def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)
  def sha256(message: Array[Byte]): Array[Byte]     = Sha256.hash(message)

  override def merkleVerify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean =
    Merkle.verify(rootBytes, proofBytes, valueBytes)

  // Math functions
  def pow(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: BaseGlobal.Rounds) : Either[String, Long] = (Try {
        val base = BD.valueOf(b, bp.toInt)
        val exp = BD.valueOf(e, ep.toInt)
        val res = BigDecimalMath.pow(base, exp, MathContext.DECIMAL128)
        res.setScale(rp.toInt, roundMode(round)).unscaledValue.longValueExact
      }).toEither.left.map(_.toString)

  def log(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: BaseGlobal.Rounds) : Either[String, Long] = (Try {
        val base = BD.valueOf(b, bp.toInt)
        val exp = BD.valueOf(e, ep.toInt)
        val res = BigDecimalMath.log(base, MathContext.DECIMAL128).divide(BigDecimalMath.log(exp, MathContext.DECIMAL128), MathContext.DECIMAL128)
        res.setScale(rp.toInt, roundMode(round)).unscaledValue.longValueExact
      }).toEither.left.map(_.toString)

  private val client = new SttpClient()
  override def requestNode(url: String): Future[NodeResponse] =
    client.requestNode(url)
}
