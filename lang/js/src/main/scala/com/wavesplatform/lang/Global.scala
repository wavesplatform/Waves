package com.wavesplatform.lang

import java.math.{BigDecimal, BigInteger}

import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}
import scala.util.Try

object Global extends BaseGlobal {
  def base58Encode(input: Array[Byte]): Either[String, String] = Right(impl.Global.base58Encode(toBuffer(input)))
  override def base58Decode(input: String, limit: Int): Either[String, Array[Byte]] =
    for {
      _ <- Either.cond(input.length <= limit, {}, s"Input is too long (${input.length}), limit is $limit")
      x <- impl.Global
        .base58Decode(input)
        .toOption
        .map(toArray)
        .toRight("Cannot decode")
    } yield x

  override def base64Encode(input: Array[Byte]): Either[String, String] = Right(impl.Global.base64Encode(toBuffer(input)))
  override def base64Decode(input: String, limit: Int): Either[String, Array[Byte]] =
    for {
      _ <- Either.cond(input.length <= limit, {}, s"Input is too long (${input.length}), limit is $limit")
      x <- impl.Global
        .base64Decode(input)
        .toOption
        .map(toArray)
        .toRight("Cannot decode")
    } yield x

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean =
    impl.Global.curve25519verify(toBuffer(message), toBuffer(sig), toBuffer(pub))


  override def rsaVerify(alg: DigestAlgorithm, message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean =
    impl.Global.rsaVerify(alg, toBuffer(message), toBuffer(sig), toBuffer(pub))

  def keccak256(message: Array[Byte]): Array[Byte]  = hash(message)(impl.Global.keccak256)
  def blake2b256(message: Array[Byte]): Array[Byte] = hash(message)(impl.Global.blake2b256)
  def sha256(message: Array[Byte]): Array[Byte]     = hash(message)(impl.Global.sha256)

  private def toArray(xs: ArrayBuffer): Array[Byte] = new Int8Array(xs).toArray
  private def hash(message: Array[Byte])(f: ArrayBuffer => ArrayBuffer): Array[Byte] = toArray(f(toBuffer(message)))

  def toBuffer(xs: Array[Byte]): ArrayBuffer = {
    val r = new Int8Array(xs.length)
    r.set(xs.toJSArray)
    r.buffer
  }

  override def merkleVerify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean =
    impl.Global.merkleVerify(toBuffer(rootBytes), toBuffer(proofBytes), toBuffer(valueBytes))

  override def pow(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: BaseGlobal.Rounds): Either[String, Long] =
    calcScaled(Math.pow)(b, bp, e, ep, rp, round)

  override def log(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: BaseGlobal.Rounds): Either[String, Long] =
    calcScaled(Math.log(_) / Math.log(_))(b, bp, e, ep, rp, round)

  private def calcScaled(calc: (Double, Double) => Double)(
    base:          Long,
    baseScale:     Long,
    exponent:      Long,
    exponentScale: Long,
    resultScale:   Long,
    round:         BaseGlobal.Rounds,
  ): Either[String, Long] =
    tryEi {
      val result = calc(
        base     * Math.pow(10, -baseScale),
        exponent * Math.pow(10, -exponentScale)
      )
      unscaled(result, resultScale, round)
    }

  private def tryEi[R](r: => Either[String, R]): Either[String, R] =
    Try(r)
      .toEither
      .leftMap(e => if (e.getMessage != null) e.getMessage else e.toString)
      .flatten

  private def unscaled(
    value: Double,
    scale: Long,
    round: BaseGlobal.Rounds
  ): Either[String, Long] = {
    val decimal =
      if (value.toLong.toDouble == value && value - 1 < Long.MaxValue) BigDecimal.valueOf(value.toLong)
      else BigDecimal.valueOf(value)

    decimal
      .setScale(scale.toInt, roundMode(round))
      .unscaledValue
      .longExact
  }

  implicit class BigIntOps(v: BigInteger) {
    // absent in scala.js BigInteger
    def longExact: Either[String, Long] =
      Either.cond(
        v.bitLength <= 63,
        v.longValue,
        "BigInteger out of long range"
      )
  }

  override def requestNode(url: String): Future[NodeResponse] =
    impl.Global.httpGet(js.Dynamic.literal(url = url))
     .toFuture
     .map(r => NodeResponse(r.status.asInstanceOf[Int], r.body.asInstanceOf[String]))
}
