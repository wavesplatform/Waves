package com.wavesplatform.db

import com.google.common.primitives.Ints
import com.wavesplatform.common.utils.EitherExt2

import scala.collection.generic.CanBuildFrom
import scala.util.Try

case class CodecFailure(reason: String) {
  override def toString: String = s"codec failure: $reason"
}

case class DecodeResult[A](length: Int, value: A)

trait Codec[A] {
  def encode(value: A): Array[Byte]

  def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[A]]
}

object Codec {
  val SignatureLength: Int    = 64
  val TrueBytes: Array[Byte]  = Array[Byte](1.toByte)
  val FalseBytes: Array[Byte] = Array[Byte](0.toByte)
}

object SeqCodec {
  def apply[A](valueCodec: Codec[A]): ColCodec[Seq, A] = ColCodec(valueCodec)
}

case class ColCodec[Col[BB] <: TraversableOnce[BB], A](valueCodec: Codec[A])(implicit cbf: CanBuildFrom[Col[A], A, Col[A]]) extends Codec[Col[A]] {
  override def encode(value: Col[A]): Array[Byte] = {
    val builder = Array.newBuilder[Byte]
    value.foreach[Unit] { item: A =>
      builder.++=(valueCodec.encode(item))
    }
    val bytes  = builder.result()
    val len    = bytes.length
    val result = new Array[Byte](Ints.BYTES + len)
    System.arraycopy(Ints.toByteArray(value.size), 0, result, 0, Ints.BYTES)
    System.arraycopy(bytes, 0, result, Ints.BYTES, len)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Col[A]]] = {
    val n = Try(Ints.fromByteArray(bytes.take(Ints.BYTES))).toEither.left.map(e => CodecFailure(e.getMessage))
    if (n.isRight) {
      val expectedLength = n.explicitGet()
      val builder        = cbf()
      var i              = Ints.BYTES
      var error          = false
      while (i < bytes.length && !error) {
        val r = valueCodec.decode(bytes.slice(i, bytes.length))
        if (r.isRight) {
          val rr = r.explicitGet()
          i = i + rr.length
          builder.+=(rr.value)
        } else {
          error = true
        }
      }
      val result = builder.result()
      Either.cond(!error && expectedLength == result.size, DecodeResult(i, result), CodecFailure(s"failed to deserialize $expectedLength items"))
    } else Left(n.left.get)
  }
}
