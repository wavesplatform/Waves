package com.wavesplatform.db

import com.google.common.base.Charsets
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.state.ByteStr
import scorex.transaction.AssetId

import scala.collection.generic.CanBuildFrom
import scala.util.Try

case class CodecFailure(reason: String) {
  override def toString: String = s"codec failure: $reason"
}

case class DecodeResult[A](length: Int, value: A)

trait Codec[A] {

  import Codec._

  def encode(value: A): Array[Byte]

  def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[A]]

  protected def decodeShort(bytes: Array[Byte]): Either[CodecFailure, Short] =
    Try(Shorts.fromByteArray(bytes)).toEither.left.map(e => CodecFailure(e.getMessage))

  protected def decodeInt(bytes: Array[Byte]): Either[CodecFailure, Int] =
    Try(Ints.fromByteArray(bytes)).toEither.left.map(e => CodecFailure(e.getMessage))

  protected def decodeLong(bytes: Array[Byte]): Either[CodecFailure, Long] =
    Try(Longs.fromByteArray(bytes)).toEither.left.map(e => CodecFailure(e.getMessage))

  protected def encodeBoolean(value: Boolean): Array[Byte] = if (value) TrueBytes else FalseBytes

  protected def decodeBoolean(bytes: Array[Byte]): Either[CodecFailure, Boolean] =
    Try(bytes.take(1).sameElements(TrueBytes)).toEither.left.map(e => CodecFailure(e.getMessage))

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

case class Tuple2Codec[A, B](aCodec: Codec[A], bCodec: Codec[B]) extends Codec[(A, B)] {
  override def encode(value: (A, B)): Array[Byte] = {
    val builder = Array.newBuilder[Byte]
    val (a, b)  = value
    builder ++= aCodec.encode(a)
    builder ++= bCodec.encode(b)
    builder.result()
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[(A, B)]] =
    for {
      a <- aCodec.decode(bytes)
      b <- bCodec.decode(bytes.slice(a.length, bytes.length))
    } yield DecodeResult(a.length + b.length, (a.value, b.value))
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
      val expectedLength = n.right.get
      val builder        = cbf()
      var i              = Ints.BYTES
      var error          = false
      while (i < bytes.length && !error) {
        val r = valueCodec.decode(bytes.slice(i, bytes.length))
        if (r.isRight) {
          val rr = r.right.get
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

case class OptionCodec[A](valueCodec: Codec[A]) extends Codec[Option[A]] {
  override def encode(value: Option[A]): Array[Byte] = value match {
    case Some(x) => Array.concat(encodeBoolean(true), valueCodec.encode(x))
    case None    => encodeBoolean(false)
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Option[A]]] = {
    decodeBoolean(bytes).flatMap {
      case true =>
        valueCodec.decode(bytes.slice(1, bytes.length)).map { x =>
          DecodeResult(1 + x.length, Some(x.value))
        }
      case false => Right(DecodeResult(1, None))
    }
  }
}

object StringCodec extends Codec[String] {
  override def encode(value: String): Array[Byte] = {
    val bytes  = value.getBytes(Charsets.UTF_8)
    val len    = bytes.length
    val result = new Array[Byte](Ints.BYTES + len)
    System.arraycopy(Ints.toByteArray(len), 0, result, 0, Ints.BYTES)
    System.arraycopy(bytes, 0, result, Ints.BYTES, len)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[String]] = {
    for {
      s <- decodeInt(bytes.take(Ints.BYTES))
      a = bytes.slice(Ints.BYTES, Ints.BYTES + s)
      _ <- Either.cond(a.length == s, (), CodecFailure("incorrect string byte representation length"))
      v <- Try(new String(a, Charsets.UTF_8)).toEither.left.map(e => CodecFailure(e.getMessage))
    } yield DecodeResult(Ints.BYTES + s, v)
  }
}

object StringSeqCodec extends Codec[Seq[String]] {
  val itemsCodec = SeqCodec(StringCodec)

  override def encode(value: Seq[String]): Array[Byte] = itemsCodec.encode(value)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Seq[String]]] = itemsCodec.decode(bytes)
}

object OrderToTxIdsCodec extends Codec[Set[String]] {
  private val itemsCodec = SeqCodec(StringCodec)

  override def encode(value: Set[String]): Array[Byte] = itemsCodec.encode(value.toSeq)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Set[String]]] =
    itemsCodec.decode(bytes).right.map(r => DecodeResult(r.length, r.value.toSet))
}

object OrderIdsCodec extends Codec[Array[String]] {
  private val itemsCodec = SeqCodec(StringCodec)

  override def encode(value: Array[String]): Array[Byte] = itemsCodec.encode(value.toSeq)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Array[String]]] =
    itemsCodec.decode(bytes).right.map(r => DecodeResult(r.length, r.value.toArray))
}

object AssetIdOrderIdCodec extends Tuple2Codec(OptionCodec[AssetId](ByteStrCodec), StringCodec)

object AssetIdOrderIdSetCodec extends ColCodec[Set, (Option[AssetId], String)](AssetIdOrderIdCodec)

object PortfolioItemCodec extends Codec[(String, Long)] {
  override def encode(value: (String, Long)): Array[Byte] = {
    val r      = StringCodec.encode(value._1)
    val len    = r.length
    val result = new Array[Byte](len + Longs.BYTES)
    System.arraycopy(r, 0, result, 0, len)
    System.arraycopy(Longs.toByteArray(value._2), 0, result, len, Longs.BYTES)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[(String, Long)]] = {
    for {
      r  <- StringCodec.decode(bytes)
      v2 <- decodeLong(bytes.slice(r.length, r.length + Longs.BYTES))
    } yield DecodeResult(r.length + Longs.BYTES, (r.value, v2))
  }
}

object PortfolioCodec extends Codec[Map[String, Long]] {
  private val itemsCodec = SeqCodec(PortfolioItemCodec)

  override def encode(value: Map[String, Long]): Array[Byte] = itemsCodec.encode(value.toSeq)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Map[String, Long]]] =
    itemsCodec.decode(bytes).right.map(r => DecodeResult(r.length, r.value.toMap))
}

object ByteStrCodec extends Codec[ByteStr] {
  override def encode(value: ByteStr): Array[Byte] = {
    val len    = value.arr.length
    val result = new Array[Byte](Ints.BYTES + len)
    System.arraycopy(Ints.toByteArray(len), 0, result, 0, Ints.BYTES)
    System.arraycopy(value.arr, 0, result, Ints.BYTES, len)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[ByteStr]] = {
    for {
      l <- decodeInt(bytes.take(Ints.BYTES))
      a = bytes.slice(Ints.BYTES, Ints.BYTES + l)
      _ <- Either.cond(a.length == l, (), CodecFailure("incorrect ByteStr length"))
    } yield DecodeResult(Ints.BYTES + l, ByteStr(a))
  }
}

object ShortCodec extends Codec[Short] {
  override def encode(value: Short): Array[Byte] = Shorts.toByteArray(value)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Short]] = {
    for {
      v <- Try(Shorts.fromByteArray(bytes.take(Shorts.BYTES))).toEither.left.map(e => CodecFailure(e.getMessage))
    } yield DecodeResult(Shorts.BYTES, v)
  }
}

object ShortSeqCodec extends Codec[Seq[Short]] {
  private val itemsCodec = SeqCodec(ShortCodec)

  override def encode(value: Seq[Short]): Array[Byte] = itemsCodec.encode(value)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Seq[Short]]] = itemsCodec.decode(bytes)
}

object KeyCodec extends Codec[ByteStr] {
  private val KeySize = 8

  override def encode(value: ByteStr): Array[Byte] = value.arr.take(KeySize)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[ByteStr]] = {
    val a = bytes.take(KeySize)
    Either.cond(a.length == KeySize, DecodeResult(KeySize, ByteStr(a)), CodecFailure("incorrect key size"))
  }
}

object KeySeqCodec extends Codec[Seq[ByteStr]] {
  val itemsCodec = SeqCodec(KeyCodec)

  override def encode(value: Seq[ByteStr]): Array[Byte] = itemsCodec.encode(value)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Seq[ByteStr]]] = itemsCodec.decode(bytes)
}

object Id32Codec extends Codec[ByteStr] {
  private val IdSize = 32

  override def encode(value: ByteStr): Array[Byte] = value.arr.take(IdSize)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[ByteStr]] = {
    val a = bytes.take(IdSize)
    Either.cond(a.length == IdSize, DecodeResult(IdSize, ByteStr(a)), CodecFailure("incorrect id32 size"))
  }
}

object Id32SeqCodec extends Codec[Seq[ByteStr]] {
  val itemsCodec = SeqCodec(Id32Codec)

  override def encode(value: Seq[ByteStr]): Array[Byte] = itemsCodec.encode(value)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Seq[ByteStr]]] = itemsCodec.decode(bytes)
}
