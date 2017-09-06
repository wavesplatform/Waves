package com.wavesplatform.db

import com.google.common.base.Charsets
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.state2.{AssetInfo, ByteStr, VolumeAndFee}
import scorex.account.Alias

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
  val SignatureLength: Int = 64
  val TrueBytes: Array[Byte] = Array[Byte](1.toByte)
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
      height <- Try(Ints.fromByteArray(arr.take(Ints.BYTES))).toEither.left.map(e => CodecFailure(e.getMessage))
      signature <- Either.cond(arr.length >= len, arr.slice(Ints.BYTES, len), CodecFailure("not enough bytes for signature"))
    } yield DecodeResult(len, BlockCheckpoint(height, signature))
  }
}

object CheckpointCodec extends Codec[Checkpoint] {
  private val itemsCodec = SeqCodec(BlockCheckpointCodec)

  override def encode(value: Checkpoint): Array[Byte] = {
    val r = itemsCodec.encode(value.items)
    val result = new Array[Byte](Codec.SignatureLength + r.length)
    System.arraycopy(value.signature, 0, result, 0, Codec.SignatureLength)
    System.arraycopy(r, 0, result, Codec.SignatureLength, r.length)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Checkpoint]] = {
    val signature = bytes.take(Codec.SignatureLength)
    for {
      _ <- Either.cond(signature.length == Codec.SignatureLength, (), CodecFailure("incorrect signature length"))
      items <- itemsCodec.decode(bytes.slice(Codec.SignatureLength, bytes.length))
    } yield DecodeResult(Codec.SignatureLength + items.length, Checkpoint(items.value, signature))
  }
}

case class SeqCodec[A](valueCodec: Codec[A]) extends Codec[Seq[A]] {
  override def encode(value: Seq[A]): Array[Byte] = {
    val builder = Array.newBuilder[Byte]
    value.foreach { item =>
      builder.++=(valueCodec.encode(item))
    }
    val bytes = builder.result()
    val len = bytes.length
    val result = new Array[Byte](Ints.BYTES + len)
    System.arraycopy(Ints.toByteArray(value.length), 0, result, 0, Ints.BYTES)
    System.arraycopy(bytes, 0, result, Ints.BYTES, len)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Seq[A]]] = {
    val n = Try(Ints.fromByteArray(bytes.take(Ints.BYTES))).toEither.left.map(e => CodecFailure(e.getMessage))
    if (n.isRight) {
      val expectedLength = n.right.get
      val builder = Seq.newBuilder[A]
      var i = Ints.BYTES
      var error = false
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
      Either.cond(!error && expectedLength == result.length, DecodeResult(i, result), CodecFailure(s"failed to deserialize $expectedLength items"))
    } else Left(n.left.get)
  }
}

object WavesBalanceValueCodec extends Codec[(Long, Long, Long)] {
  override def encode(value: (Long, Long, Long)): Array[Byte] = {
    val result = new Array[Byte](3 * Longs.BYTES)
    System.arraycopy(Longs.toByteArray(value._1), 0, result, 0, Longs.BYTES)
    System.arraycopy(Longs.toByteArray(value._2), 0, result, Longs.BYTES, Longs.BYTES)
    System.arraycopy(Longs.toByteArray(value._3), 0, result, 2 * Longs.BYTES, Longs.BYTES)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[(Long, Long, Long)]] = {
    for {
      v1 <- decodeLong(bytes.take(Longs.BYTES))
      v2 <- decodeLong(bytes.slice(Longs.BYTES, Longs.BYTES * 2))
      v3 <- decodeLong(bytes.slice(Longs.BYTES * 2, Longs.BYTES * 3))
    } yield DecodeResult(Longs.BYTES * 3, (v1, v2, v3))
  }
}

object BalanceSnapshotValueCodec extends Codec[(Int, Long, Long)] {
  override def encode(value: (Int, Long, Long)): Array[Byte] = {
    val result = new Array[Byte](Ints.BYTES + 2 * Longs.BYTES)
    System.arraycopy(Ints.toByteArray(value._1), 0, result, 0, Ints.BYTES)
    System.arraycopy(Longs.toByteArray(value._2), 0, result, Ints.BYTES, Longs.BYTES)
    System.arraycopy(Longs.toByteArray(value._3), 0, result, Ints.BYTES + Longs.BYTES, Longs.BYTES)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[(Int, Long, Long)]] = {
    for {
      v1 <- decodeInt(bytes.take(Ints.BYTES))
      v2 <- decodeLong(bytes.slice(Ints.BYTES, Ints.BYTES + Longs.BYTES))
      v3 <- decodeLong(bytes.slice(Ints.BYTES + Longs.BYTES, Ints.BYTES + 2 * Longs.BYTES))
    } yield DecodeResult(Ints.BYTES + 2 * Longs.BYTES, (v1, v2, v3))
  }
}

object OrderFillInfoValueCodec extends Codec[VolumeAndFee] {
  override def encode(value: VolumeAndFee): Array[Byte] = {
    val result = new Array[Byte](2 * Longs.BYTES)
    System.arraycopy(Longs.toByteArray(value.volume), 0, result, 0, Longs.BYTES)
    System.arraycopy(Longs.toByteArray(value.fee), 0, result, Longs.BYTES, Longs.BYTES)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[VolumeAndFee]] = {
    for {
      vol <- decodeLong(bytes.take(Longs.BYTES))
      fee <- decodeLong(bytes.slice(Longs.BYTES, 2 * Longs.BYTES))
    } yield DecodeResult(Longs.BYTES * 2, VolumeAndFee(vol, fee))
  }
}

object VoteCodec extends Codec[(Short, Int)] {
  override def encode(value: (Short, Int)): Array[Byte] = {
    val result = new Array[Byte](Shorts.BYTES + Ints.BYTES)
    System.arraycopy(Shorts.toByteArray(value._1), 0, result, 0, Shorts.BYTES)
    System.arraycopy(Ints.toByteArray(value._2), 0, result, Shorts.BYTES, Ints.BYTES)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[(Short, Int)]] = {
    for {
      v1 <- decodeShort(bytes.take(Shorts.BYTES))
      v2 <- decodeInt(bytes.slice(Shorts.BYTES, Shorts.BYTES + Ints.BYTES))
    } yield DecodeResult(Shorts.BYTES + Ints.BYTES, (v1, v2))
  }
}

object VotesMapCodec extends Codec[Map[Short, Int]] {
  private val itemsCodec = SeqCodec(VoteCodec)

  override def encode(value: Map[Short, Int]): Array[Byte] = itemsCodec.encode(value.toSeq)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Map[Short, Int]]] = itemsCodec.decode(bytes)
    .map(r => DecodeResult(r.length, r.value.toMap))
}

object TransactionsValueCodec extends Codec[(Int, Array[Byte])] {
  override def encode(value: (Int, Array[Byte])): Array[Byte] = {
    val len = value._2.length
    val result = new Array[Byte](2 * Ints.BYTES + len)
    System.arraycopy(Ints.toByteArray(value._1), 0, result, 0, Ints.BYTES)
    System.arraycopy(Ints.toByteArray(len), 0, result, Ints.BYTES, Ints.BYTES)
    System.arraycopy(value._2, 0, result, 2 * Ints.BYTES, len)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[(Int, Array[Byte])]] = {
    for {
      v1 <- decodeInt(bytes.take(Ints.BYTES))
      l <- decodeInt(bytes.slice(Ints.BYTES, 2 * Ints.BYTES))
      a = bytes.slice(2 * Ints.BYTES, 2 * Ints.BYTES + l)
      _ <- Either.cond(a.length == l, (), CodecFailure("incorrect array length"))
    } yield DecodeResult(2 * Ints.BYTES + l, (v1, a))
  }
}

object AssetInfoCodec extends Codec[AssetInfo] {
  override def encode(value: AssetInfo): Array[Byte] = {
    val result = new Array[Byte](1 + Longs.BYTES)
    System.arraycopy(encodeBoolean(value.isReissuable), 0, result, 0, 1)
    System.arraycopy(Longs.toByteArray(???), 0, result, 1, Longs.BYTES)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[AssetInfo]] = {
    for {
      v1 <- decodeBoolean(bytes.take(1))
      v2 <- decodeLong(bytes.slice(1, Longs.BYTES + 1))
    } yield DecodeResult(Longs.BYTES + 1, AssetInfo(v1, v2))
  }
}

object AliasCodec extends Codec[Alias] {
  override def encode(value: Alias): Array[Byte] = ByteStrCodec.encode(value.bytes)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Alias]] = {
    for {
      r <- ByteStrCodec.decode(bytes)
      a <- Alias.fromBytes(r.value.arr).left.map(e => CodecFailure(e.toString))
    } yield DecodeResult(r.length, a)
  }
}

object AliasSeqCodec extends Codec[Seq[Alias]] {
  private val itemsCodec = SeqCodec(AliasCodec)

  override def encode(value: Seq[Alias]): Array[Byte] = itemsCodec.encode(value)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Seq[Alias]]] = itemsCodec.decode(bytes)
}

object StringCodec extends Codec[String] {
  override def encode(value: String): Array[Byte] = {
    val bytes = value.getBytes(Charsets.UTF_8)
    val len = bytes.length
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

object OrderToTxIdsCodec extends Codec[Set[String]] {
  private val itemsCodec = SeqCodec(StringCodec)

  override def encode(value: Set[String]): Array[Byte] = itemsCodec.encode(value.toSeq)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Set[String]]] = itemsCodec.decode(bytes)
    .right.map(r => DecodeResult(r.length, r.value.toSet))
}

object OrderIdsCodec extends Codec[Array[String]] {
  private val itemsCodec = SeqCodec(StringCodec)

  override def encode(value: Array[String]): Array[Byte] = itemsCodec.encode(value.toSeq)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Array[String]]] = itemsCodec.decode(bytes)
    .right.map(r => DecodeResult(r.length, r.value.toArray))
}

object PortfolioItemCodec extends Codec[(String, Long)] {
  override def encode(value: (String, Long)): Array[Byte] = {
    val r = StringCodec.encode(value._1)
    val len = r.length
    val result = new Array[Byte](len + Longs.BYTES)
    System.arraycopy(r, 0, result, 0, len)
    System.arraycopy(Longs.toByteArray(value._2), 0, result, len, Longs.BYTES)
    result
  }

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[(String, Long)]] = {
    for {
      r <- StringCodec.decode(bytes)
      v2 <- decodeLong(bytes.slice(r.length, r.length + Longs.BYTES))
    } yield DecodeResult(r.length + Longs.BYTES, (r.value, v2))
  }
}

object PortfolioCodec extends Codec[Map[String, Long]] {
  private val itemsCodec = SeqCodec(PortfolioItemCodec)

  override def encode(value: Map[String, Long]): Array[Byte] = itemsCodec.encode(value.toSeq)

  override def decode(bytes: Array[Byte]): Either[CodecFailure, DecodeResult[Map[String, Long]]] = itemsCodec.decode(bytes)
    .right.map(r => DecodeResult(r.length, r.value.toMap))
}

object ByteStrCodec extends Codec[ByteStr] {
  override def encode(value: ByteStr): Array[Byte] = {
    val len = value.arr.length
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
