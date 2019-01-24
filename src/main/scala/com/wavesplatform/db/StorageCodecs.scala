package com.wavesplatform.db

import com.google.common.primitives.Ints
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
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

object BlockCheckpointCodec extends Codec[BlockCheckpoint] {
  override def encode(bcp: BlockCheckpoint): Array[Byte] = {
    val result = new Array[Byte](Ints.BYTES + Codec.SignatureLength)
    System.arraycopy(Ints.toByteArray(bcp.height), 0, result, 0, Ints.BYTES)
    System.arraycopy(bcp.signature, 0, result, Ints.BYTES, Codec.SignatureLength)
    result
  }

  override def decode(arr: Array[Byte]): Either[CodecFailure, DecodeResult[BlockCheckpoint]] = {
    val len = Ints.BYTES + Codec.SignatureLength
    for {
      height    <- Try(Ints.fromByteArray(arr.take(Ints.BYTES))).toEither.left.map(e => CodecFailure(e.getMessage))
      signature <- Either.cond(arr.length >= len, arr.slice(Ints.BYTES, len), CodecFailure("not enough bytes for signature"))
    } yield DecodeResult(len, BlockCheckpoint(height, signature))
  }
}

object CheckpointCodec extends Codec[Checkpoint] {
  private val itemsCodec = SeqCodec(BlockCheckpointCodec)

  override def encode(value: Checkpoint): Array[Byte] = {
    val r      = itemsCodec.encode(value.items)
    val result = new Array[Byte](Codec.SignatureLength + r.length)
    System.arraycopy(value.signature, 0, result, 0, Codec.SignatureLength)
    System.arraycopy(r, 0, result, Codec.SignatureLength, r.length)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Checkpoint]] = {
    val signature = bytes.take(Codec.SignatureLength)
    for {
      _     <- Either.cond(signature.length == Codec.SignatureLength, (), CodecFailure("incorrect signature length"))
      items <- itemsCodec.decode(bytes.slice(Codec.SignatureLength, bytes.length))
    } yield DecodeResult(Codec.SignatureLength + items.length, Checkpoint(items.value, signature))
  }
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
