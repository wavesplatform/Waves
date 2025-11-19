package com.wavesplatform

import cats.Id
import cats.implicits.*
import com.google.common.primitives.Ints
import com.wavesplatform.common.state.ByteStr
import play.api.libs.json.*

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.Try

package object state {
  def safeSum(x: Long, y: Long, source: String): Either[String, Long] =
    Try(Math.addExact(x, y)).toEither.leftMap(_ => s"$source sum overflow")

  implicit val safeSummarizer: Summarizer[[X] =>> Either[String, X]] = safeSum(_, _, _)
  implicit val unsafeSummarizer: Summarizer[Id]                      = (x, y, _) => x + y

  implicit class Cast[A](a: A) {
    def cast[B: ClassTag]: Option[B] = {
      a match {
        case b: B => Some(b)
        case _    => None
      }
    }
  }

  object Height {
    def apply(h: Int): Height                = h
    def seq(ints: Seq[Int]): Seq[Height]     = ints
    def ints(heights: Seq[Height]): Seq[Int] = heights

    extension (h: Height) {
      def toInt: Int               = h
      def toByteArray: Array[Byte] = Ints.toByteArray(h)
      def +(that: Int): Height     = h + that
      def -(that: Int): Height     = h - that

      @targetName("minusHeight")
      def -(that: Height): Int = h - that

      infix def to(end: Height): Range.Inclusive = Range.inclusive(h, end)

      def max(that: Int): Height = that.max(h)
      @targetName("maxHeight")
      def max(that: Height): Height = that.max(h)
    }

    given Ordering[Height]                    = Ordering[Int]
    given Conversion[Height, Ordered[Height]] = scala.math.Ordered.orderingToOrdered(_)

    given Writes[Height] = Writes.IntWrites
  }
  opaque type Height = Int

  object TxNum {
    def apply(s: Short): TxNum = s
    extension (n: TxNum) {
      def toShort: Short  = n
      def unary_- : TxNum = (-n).toShort
    }
    given Ordering[TxNum]                   = Ordering[Short]
    given Conversion[TxNum, Ordered[TxNum]] = scala.math.Ordered.orderingToOrdered(_)
  }

  opaque type TxNum = Short

  object TransactionId {
    def apply(bs: ByteStr): TransactionId = bs

    implicit val format: Format[TransactionId] = Format[TransactionId](
      com.wavesplatform.utils.byteStrFormat.map(this(_)),
      Writes(com.wavesplatform.utils.byteStrFormat.writes)
    )

    extension (txId: TransactionId) {
      def arr: Array[Byte] = txId.arr
      def byteStr: ByteStr = txId
    }
  }
  opaque type TransactionId = ByteStr
}
