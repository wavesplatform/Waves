package com.wavesplatform.lang

import com.wavesplatform.lang.v1.BaseGlobal

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

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

  def keccak256(message: Array[Byte]): Array[Byte]  = hash(message)(impl.Global.keccak256)
  def blake2b256(message: Array[Byte]): Array[Byte] = hash(message)(impl.Global.blake2b256)
  def sha256(message: Array[Byte]): Array[Byte]     = hash(message)(impl.Global.sha256)

  private def hash(message: Array[Byte])(f: ArrayBuffer => ArrayBuffer): Array[Byte] = toArray(f(toBuffer(message)))

  def toBuffer(xs: Array[Byte]): ArrayBuffer = {
    val r = new Int8Array(xs.length)
    r.set(xs.toJSArray)
    r.buffer
  }

  private def toArray(xs: ArrayBuffer): Array[Byte] = new Int8Array(xs).toArray
}
