package com.wavesplatform.database

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{AddressId, Height, TxNum}
import simulacrum.{op, typeclass}

object KeyDsl {
  sealed trait KeyBytes {
    def bytes: Array[Byte]
  }

  object KeyBytes {
    final case class Strict(bytes: Array[Byte]) extends KeyBytes
    final case class SeqBS(bytesSeq: KeyBytes*) extends KeyBytes {
      private lazy val arrays: Seq[Array[Byte]] = bytesSeq.flatMap {
        case Strict(bs) => Seq(bs)
        case seq: SeqBS => seq.arrays
      }

      override lazy val bytes: Array[Byte] = Bytes.concat(arrays: _*)
    }

    val empty = Strict(Array.emptyByteArray)

    implicit def toBytes(kb: KeyBytes): Array[Byte]   = kb.bytes
    implicit def fromBytes(bs: Array[Byte]): KeyBytes = Strict(bs)
    implicit def fromByteStr(bs: ByteStr): KeyBytes   = Strict(bs)
  }

  @typeclass
  trait KeyW[T] {
    @op("toKeyBytes") def toBytes(v: T): KeyBytes
  }

  @typeclass
  trait KeyR[T] {
    def fromBytes(bs: KeyBytes): T
  }

  @typeclass
  trait KeyRW[T] extends KeyR[T] with KeyW[T]

  trait FixedSize {
    def bytesSize: Int
  }

  trait KeyFS[T] extends KeyRW[T] with FixedSize

  object Implicits extends LowPriorityRWs {
    implicit object BytesRW extends KeyRW[Array[Byte]] {
      override def toBytes(v: Array[Byte]): KeyBytes    = v
      override def fromBytes(bs: KeyBytes): Array[Byte] = bs
    }

    implicit object ByteStrRW extends KeyRW[ByteStr] {
      override def toBytes(v: ByteStr): KeyBytes    = v.arr
      override def fromBytes(bs: KeyBytes): ByteStr = ByteStr(bs.bytes)
    }

    implicit object StringRW extends KeyRW[String] {
      override def toBytes(v: String): KeyBytes = v.getBytes(StandardCharsets.UTF_8)

      override def fromBytes(bs: KeyBytes): String = new String(bs: Array[Byte], StandardCharsets.UTF_8)
    }

    implicit object IntRW extends KeyFS[Int] {
      override def toBytes(v: Int): KeyBytes    = Ints.toByteArray(v)
      override def fromBytes(bs: KeyBytes): Int = Ints.fromByteArray(bs)
      override def bytesSize: Int               = 4
    }

    implicit object ShortRW extends KeyFS[Short] {
      override def toBytes(v: Short): KeyBytes    = Shorts.toByteArray(v)
      override def fromBytes(bs: KeyBytes): Short = Shorts.fromByteArray(bs)
      override def bytesSize: Int                 = 2
    }

    implicit object LongRW extends KeyFS[Long] {
      override def toBytes(v: Long): KeyBytes    = Longs.toByteArray(v)
      override def fromBytes(bs: KeyBytes): Long = Longs.fromByteArray(bs)
      override def bytesSize: Int                = 8
    }

    implicit object AddressIdRW extends KeyFS[AddressId] {
      override def toBytes(v: AddressId): KeyBytes    = AddressId.toBytes(v)
      override def fromBytes(bs: KeyBytes): AddressId = AddressId.fromBytes(bs)
      override def bytesSize: Int                     = 4
    }

    import supertagged._

    def taggedCFS[T <: TaggedType[_]](v: T)(implicit ev: KeyFS[v.Raw]): KeyFS[v.Type] = ev.asInstanceOf[KeyFS[v.Type]]

    implicit val HeightRW: KeyFS[Height] = taggedCFS(Height)
    implicit val TxNumRW: KeyFS[TxNum] = taggedCFS(TxNum)
  }

  trait LowPriorityRWs {
    import shapeless._

    implicit def hnilRW: KeyFS[HNil] = new KeyFS[HNil] {
      override def bytesSize: Int                = 0
      override def toBytes(v: HNil): KeyBytes    = Array.emptyByteArray
      override def fromBytes(bs: KeyBytes): HNil = HNil
    }

    implicit def hconsRW[H, T <: HList](implicit hev: Lazy[KeyFS[H]], tev: KeyFS[T]): KeyFS[H :: T] = new KeyFS[H :: T] {
      override def bytesSize: Int = hev.value.bytesSize + tev.bytesSize
      override def toBytes(v: H :: T): KeyBytes = {
        KeyBytes.SeqBS(hev.value.toBytes(v.head), tev.toBytes(v.tail))
      }
      override def fromBytes(bs: KeyBytes): H :: T = {
        val (hb, tb) = bs.bytes.splitAt(hev.value.bytesSize)
        val headV = hev.value.fromBytes(hb)
        val tailV    = tev.fromBytes(tb)
        headV :: tailV
      }
    }

    implicit def genericRW[T, L <: HList](implicit g: Generic.Aux[T, L], lev: KeyFS[L]): KeyFS[T] =
      new KeyFS[T] {
        override def bytesSize: Int             = lev.bytesSize
        override def toBytes(v: T): KeyBytes    = lev.toBytes(g.to(v))
        override def fromBytes(bs: KeyBytes): T = g.from(lev.fromBytes(bs))
      }

    implicit def hconsW[H, T <: HList](implicit hev: Lazy[KeyW[H]], tev: KeyW[T]): KeyW[H :: T] = (v: H :: T) => {
      KeyBytes.SeqBS(hev.value.toBytes(v.head), tev.toBytes(v.tail))
    }

    implicit def genericW[T, L <: HList](implicit g: Generic.Aux[T, L], lev: KeyW[L]): KeyW[T] =
      (v: T) => lev.toBytes(g.to(v))
  }
}
