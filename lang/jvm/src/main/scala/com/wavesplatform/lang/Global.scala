package com.wavesplatform.lang

import java.math.{MathContext, BigDecimal => BD}
import java.security.spec.InvalidKeySpecException
import cats.implicits._
import ch.obermuhlner.math.big.BigDecimalMath
import com.google.common.base.Utf8
import com.google.common.io.BaseEncoding
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse
import com.wavesplatform.utils.Merkle
import com.wavesplatform.zwaves.bls12.{Groth16 => Bls12Groth16}
import com.wavesplatform.zwaves.bn256.{Groth16 => Bn256Groth16}
import org.web3j.crypto.Sign
import org.web3j.crypto.Sign.SignatureData
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.annotation.tailrec
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

  private val base16Encoder: BaseEncoding = BaseEncoding.base16().lowerCase()

  override def base16EncodeImpl(input: Array[Byte]): Either[String, String] =
    toEither(base16Encoder.encode(input))

  override def base16DecodeImpl(input: String): Either[String, Array[Byte]] =
    toEither(base16Encoder.decode(input.toLowerCase))

  private def toEither[A](f: => A): Either[String, A] =
    Try(f).toEither
      .leftMap { exception =>
        val cause = findThrowableCause(exception)
        if (cause.getMessage != null) cause.getMessage
        else cause.toString
      }

  @tailrec
  private def findThrowableCause(th: Throwable): Throwable =
    if (th.getCause == null) th
    else findThrowableCause(th.getCause)

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean = Curve25519.verify(Signature(sig), message, PublicKey(pub))

  override def rsaVerify(alg: DigestAlgorithm, message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Either[String, Boolean] =
    Try(RSA.verify(alg, message, sig, pub))
      .toEither
      .leftMap {
        case err: InvalidKeySpecException => s"Invalid key base58'${Base58.encode(pub)}': ${findThrowableCause(err).getMessage}"
        case err                          => findThrowableCause(err).getMessage
      }

  def keccak256(message: Array[Byte]): Array[Byte]  = Keccak256.hash(message)
  def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)
  def sha256(message: Array[Byte]): Array[Byte]     = Sha256.hash(message)

  override def merkleVerify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean =
    Merkle.verify(rootBytes, proofBytes, valueBytes)

  // Math functions
  def pow(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: Rounding): Either[String, Long] =
    (Try {
      val base = BD.valueOf(b, bp.toInt)
      val exp  = BD.valueOf(e, ep.toInt)
      val res  = BigDecimalMath.pow(base, exp, MathContext.DECIMAL128)
      res.setScale(rp.toInt, round.mode).unscaledValue.longValueExact
    }).toEither.left.map(_.toString)

  def log(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: Rounding): Either[String, Long] =
    (Try {
      val base = BD.valueOf(b, bp.toInt)
      val exp  = BD.valueOf(e, ep.toInt)
      val res  = BigDecimalMath.log(base, MathContext.DECIMAL128).divide(BigDecimalMath.log(exp, MathContext.DECIMAL128), MathContext.DECIMAL128)
      res.setScale(rp.toInt, round.mode).unscaledValue.longValueExact
    }).toEither.left.map(_.toString)

  val bigMathContext = new MathContext(156 + 40)

  def toJBig(v: BigInt, p: Long) = BigDecimal(v).bigDecimal.multiply(BD.valueOf(1L, p.toInt))

  def powBigInt(b: BigInt, bp: Long, e: BigInt, ep: Long, rp: Long, round: Rounding): Either[String, BigInt] =
    toEither {
      val base = toJBig(b, bp)
      val exp  = toJBig(e, ep)
      val res  = BigDecimalMath.pow(base, exp, bigMathContext)
      BigInt(res.setScale(rp.toInt, round.mode).unscaledValue)
    }

  def logBigInt(b: BigInt, bp: Long, e: BigInt, ep: Long, rp: Long, round: Rounding): Either[String, BigInt] =
    toEither {
      val base = toJBig(b, bp)
      val exp  = toJBig(e, ep)
      val res  = BigDecimalMath.log(base, bigMathContext).divide(BigDecimalMath.log(exp, bigMathContext), bigMathContext)
      BigInt(res.setScale(rp.toInt, round.mode).unscaledValue)
    }

  private val client = new SttpClient()
  override def requestNode(url: String): Future[NodeResponse] =
    client.requestNode(url)

  override def groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean =
    Bls12Groth16.verify(verifyingKey, proof, inputs)

  override def bn256Groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean =
    Bn256Groth16.verify(verifyingKey, proof, inputs)

  override def ecrecover(messageHash: Array[Byte], signature: Array[Byte]): Array[Byte] = {
    // https://github.com/web3j/web3j/blob/master/crypto/src/test/java/org/web3j/crypto/ECRecoverTest.java#L43
    val signatureData = {
      val vTemp = signature(64)
      val v = if (vTemp < 27) (vTemp + 27).toByte else vTemp
      val r = signature.slice(0, 32)
      val s = signature.slice(32, 64)
      new SignatureData(v, r, s)
    }
    val pk = Sign.signedMessageHashToKey(messageHash, signatureData)
    base16Encoder.decode(pk.toString(16))
  }

  override def isIllFormed(s: String): Boolean =
    Try(Utf8.encodedLength(s)).isFailure
}
