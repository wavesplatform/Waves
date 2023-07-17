package com.wavesplatform.lang

import cats.syntax.either.*
import ch.obermuhlner.math.big.BigDecimalMath
import com.google.common.io.BaseEncoding
import com.wavesplatform.common.merkle.Merkle
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.crypto.{Blake2b256, Curve25519, Keccak256, Sha256}
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.zwaves.bls12.Groth16 as Bls12Groth16
import com.wavesplatform.zwaves.bn256.Groth16 as Bn256Groth16
import org.web3j.crypto.Sign
import org.web3j.crypto.Sign.SignatureData

import java.math.{BigInteger, MathContext, BigDecimal as BD}
import java.security.spec.InvalidKeySpecException
import scala.annotation.tailrec
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
    tryEither(base16Encoder.encode(input))

  override def base16DecodeImpl(input: String): Either[String, Array[Byte]] =
    tryEither(base16Encoder.decode(input.toLowerCase))

  private def tryEither[A](f: => A): Either[String, A] =
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

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean =
    Curve25519.verify(sig, message, pub)

  override def rsaVerify(alg: DigestAlgorithm, message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Either[String, Boolean] =
    Try(RSA.verify(alg, message, sig, pub)).toEither
      .leftMap {
        case err: InvalidKeySpecException => s"Invalid key base58'${Base58.encode(pub)}': ${findThrowableCause(err).getMessage}"
        case err                          => findThrowableCause(err).getMessage
      }

  def keccak256(message: Array[Byte]): Array[Byte]  = Keccak256.hash(message)
  def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)
  def sha256(message: Array[Byte]): Array[Byte]     = Sha256.hash(message)

  override def merkleVerify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean =
    Merkle.verify(rootBytes, proofBytes, valueBytes)

  private val longDigits     = 19
  private val longContext    = new MathContext(longDigits)
  private val oldLongContext = MathContext.DECIMAL128

  private val bigIntDigits      = 154
  private val bigMathContext    = new MathContext(bigIntDigits)
  private val oldBigMathContext = new MathContext(156 + 40)

  // Math functions
  def pow(
      base: Long,
      basePrecision: Int,
      exponent: Long,
      exponentPrecision: Int,
      resultPrecision: Int,
      round: Rounding,
      useNewPrecision: Boolean
  ): Either[String, Long] =
    tryEither {
      val baseBD  = BD.valueOf(base, basePrecision)
      val expBD   = BD.valueOf(exponent, exponentPrecision)
      val context = if (useNewPrecision) longContext else oldLongContext
      val result = if (expBD == BigDecimal(0.5).bigDecimal) {
        BigDecimalMath.sqrt(baseBD, context)
      } else {
        BigDecimalMath.pow(baseBD, expBD, context)
      }
      if (useNewPrecision)
        setScale(resultPrecision, round, context.getPrecision, result)
      else {
        val value = result.setScale(resultPrecision.toInt, round.mode).unscaledValue
        Right(BigInt(value))
      }
    }.flatten.map(_.bigInteger.longValueExact())

  def log(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: Rounding): Either[String, Long] =
    tryEither {
      val base = BD.valueOf(b, bp.toInt)
      val exp  = BD.valueOf(e, ep.toInt)
      val res  = BigDecimalMath.log(base, MathContext.DECIMAL128).divide(BigDecimalMath.log(exp, MathContext.DECIMAL128), MathContext.DECIMAL128)
      res.setScale(rp.toInt, round.mode).unscaledValue.longValueExact
    }

  def toJBig(v: BigInt, p: Long) = BigDecimal(v).bigDecimal.multiply(BD.valueOf(1L, p.toInt))

  def powBigInt(b: BigInt, bp: Long, e: BigInt, ep: Long, rp: Long, round: Rounding, useNewPrecision: Boolean): Either[String, BigInt] =
    tryEither {
      val base = toJBig(b, bp)
      val exp  = toJBig(e, ep)

      val context = if (useNewPrecision) bigMathContext else oldBigMathContext
      val res = if (exp == BigDecimal(0.5).bigDecimal) {
        BigDecimalMath.sqrt(base, context)
      } else {
        BigDecimalMath.pow(base, exp, context)
      }
      if (useNewPrecision)
        setScale(rp.toInt, round, context.getPrecision, res)
      else
        Right(BigInt(res.setScale(rp.toInt, round.mode).unscaledValue))
    }.flatten

  def logBigInt(b: BigInt, bp: Long, e: BigInt, ep: Long, rp: Long, round: Rounding): Either[String, BigInt] =
    tryEither {
      val base = toJBig(b, bp)
      val exp  = toJBig(e, ep)
      val res  = BigDecimalMath.log(base, bigMathContext).divide(BigDecimalMath.log(exp, bigMathContext), bigMathContext)
      BigInt(res.setScale(rp.toInt, round.mode).unscaledValue)
    }

  private def setScale(
      resultPrecision: Int,
      round: Rounding,
      precision: Int,
      result: java.math.BigDecimal
  ): Either[String, BigInt] = {
    val value = result.unscaledValue()
    val scale = result.scale()
    if (scale > resultPrecision)
      if (scale - resultPrecision > precision - 1)
        Right(BigInt(0))
      else
        divide(value, BigInteger.TEN.pow(scale - resultPrecision), round)
    else if (resultPrecision - scale > precision - 1)
      Left("Pow overflow")
    else
      Right(BigInt(value) * BigInteger.TEN.pow(resultPrecision - scale))
  }

  override def groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean =
    Bls12Groth16.verify(verifyingKey, proof, inputs)

  override def bn256Groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean =
    Bn256Groth16.verify(verifyingKey, proof, inputs)

  override def ecrecover(messageHash: Array[Byte], signature: Array[Byte]): Array[Byte] = {
    // https://github.com/web3j/web3j/blob/master/crypto/src/test/java/org/web3j/crypto/ECRecoverTest.java#L43
    val signatureData = {
      val vTemp = signature(64)
      val v     = if (vTemp < 27) (vTemp + 27).toByte else vTemp
      val r     = signature.slice(0, 32)
      val s     = signature.slice(32, 64)
      new SignatureData(v, r, s)
    }
    val pk = Sign.signedMessageHashToKey(messageHash, signatureData)
    base16Encoder.decode(pk.toString(16))
  }
}
