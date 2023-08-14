package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.ride.runner.caches.disk.syntax.*
import com.wavesplatform.state.Height

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.collection.Factory

trait AsBytes[T] {
  def read(from: Array[Byte]): T = read(ByteBuffer.wrap(from))
  def read(from: ByteBuffer): T

  def asBytes(x: T): Array[Byte] = {
    val r = new ByteArrayOutputStream()
    write(r, x)
    r.toByteArray
  }

  def write(output: OutputStream, x: T): Unit
}

object AsBytes {
  def apply[T](implicit r: AsBytes[T]): AsBytes[T] = r
  def mk[T](w: (OutputStream, T) => Unit, r: ByteBuffer => T): AsBytes[T] = new AsBytes[T] {
    override def write(output: OutputStream, x: T): Unit = w(output, x)
    override def read(from: ByteBuffer): T               = r(from)
  }

  implicit final class AsBytesOps[A](val self: AsBytes[A]) extends AnyVal {
    def transform[B](toB: A => B, fromB: B => A): AsBytes[B] = new AsBytes[B] {
      override def write(output: OutputStream, x: B): Unit = self.write(output, fromB(x))
      override def read(from: ByteBuffer): B               = toB(self.read(from))
    }

    def prefixed(prefix: Array[Byte]): AsBytes[A] = new AsBytes[A] {
      override def write(output: OutputStream, x: A): Unit = {
        output.write(prefix)
        self.write(output, x)
      }

      override def read(from: ByteBuffer): A = {
        val _ = from.position(from.position() + prefix.length)
        self.read(from)
      }
    }
  }

  implicit val unitAsBytes: AsBytes[Unit]    = AsBytes.mk[Unit]((_, _) => (), _ => ())
  implicit val byteAsBytes: AsBytes[Byte]    = AsBytes.mk[Byte](_.writeByte(_), _.readByte())
  implicit val shortAsBytes: AsBytes[Short]  = AsBytes.mk[Short](_.writeShort(_), _.readShort())
  implicit val intAsBytes: AsBytes[Int]      = AsBytes.mk[Int](_.writeInt(_), _.readInt())
  implicit val longAsBytes: AsBytes[Long]    = AsBytes.mk[Long](_.writeLong(_), _.readLong())
  implicit val boolAsBytes: AsBytes[Boolean] = AsBytes.mk[Boolean](_.writeBool(_), _.readBool())

  implicit def optional[T](implicit underlying: AsBytes[T]): AsBytes[Option[T]] = AsBytes.mk[Option[T]](
    w = { (os, x) =>
      os.writeBool(x.isDefined)
      x.foreach(underlying.write(os, _))
    },
    r = xs => {
      if (xs.readBool()) Some(underlying.read(xs))
      else None
    }
  )

  object byteArrayAsBytes {
    val consumeAll: AsBytes[Array[Byte]] = AsBytes.mk[Array[Byte]](
      w = _.write(_),
      r = xs => xs.readBytes(xs.remaining())
    )

    def fixed(n: Int): AsBytes[Array[Byte]] = AsBytes.mk[Array[Byte]](
      w = { (os, xs) =>
        require(xs.length == n, s"Wrong length of an array, expected: len=${xs.length} == $n")
        os.write(xs)
      },
      r = { xs => xs.readBytes(n) }
    )

    val withIntLen: AsBytes[Array[Byte]] = AsBytes.mk[Array[Byte]](
      w = { (os, xs) => os.writeWithIntLen(xs) },
      r = xs => xs.readWithIntLen()
    )
  }

  object utf8StringAsBytes {
    val consumeAll: AsBytes[String] = byteArrayAsBytes.consumeAll.transform(
      new String(_, StandardCharsets.UTF_8),
      _.getBytes(StandardCharsets.UTF_8)
    )
  }

  def tuple[A, B](aAsBytes: AsBytes[A], bAsBytes: AsBytes[B]): AsBytes[(A, B)] = AsBytes.mk[(A, B)](
    w = { (os, x) =>
      val (a, b) = x
      aAsBytes.write(os, a)
      bAsBytes.write(os, b)
    },
    r = xs => (aAsBytes.read(xs), bAsBytes.read(xs))
  )

  def tuple[A, B, C](aAsBytes: AsBytes[A], bAsBytes: AsBytes[B], cAsBytes: AsBytes[C]): AsBytes[(A, B, C)] = AsBytes.mk[(A, B, C)](
    w = { (os, x) =>
      val (a, b, c) = x
      aAsBytes.write(os, a)
      bAsBytes.write(os, b)
      cAsBytes.write(os, c)
    },
    r = xs => (aAsBytes.read(xs), bAsBytes.read(xs), cAsBytes.read(xs))
  )

  implicit def tuple2[A, B](implicit aAsBytes: AsBytes[A], bAsBytes: AsBytes[B]): AsBytes[(A, B)] = tuple(aAsBytes, bAsBytes)
  implicit def tuple3[A, B, C](implicit aAsBytes: AsBytes[A], bAsBytes: AsBytes[B], cAsBytes: AsBytes[C]): AsBytes[(A, B, C)] =
    tuple(aAsBytes, bAsBytes, cAsBytes)

  object colAsBytes {
    def consumeAll[V, C <: Seq[V]](implicit vAsBytes: AsBytes[V], factory: Factory[V, C]): AsBytes[C] = AsBytes.mk[C](
      w = { (os, xs) => xs.foreach(vAsBytes.write(os, _)) },
      r = xs => {
        val r = factory.newBuilder
        while (xs.hasRemaining) {
          r.addOne(vAsBytes.read(xs))
        }
        r.result()
      }
    )
  }

  object vecAsBytes {
    def consumeAll[V](implicit vAsBytes: AsBytes[V]): AsBytes[Vector[V]] = colAsBytes.consumeAll[V, Vector[V]]
  }

  object listAsBytes {
    def consumeAll[V](implicit vAsBytes: AsBytes[V]): AsBytes[List[V]] = colAsBytes.consumeAll[V, List[V]]
  }

  object mapAsBytes {
    def consumeAll[K: AsBytes, V: AsBytes]: AsBytes[Map[K, V]] = vecAsBytes.consumeAll[(K, V)].transform(_.toMap, _.toVector)
  }

  implicit val heightAsBytes: AsBytes[Height] = AsBytes[Int].transform(Height(_), _.toInt)

  implicit final class ByteArrayAsBytesOps(private val self: AsBytes[Array[Byte]]) extends AnyVal {
    def toByteStr: AsBytes[ByteStr] = self.transform(ByteStr(_), _.arr)
  }

//  def coerce[T, TaggedT <: TaggedType[T]#Type](implicit asBytes: AsBytes[T]): AsBytes[TaggedT] =
//    asBytes.transform(TaggedT)
}
