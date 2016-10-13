package scorex.network

import com.google.common.primitives.{Bytes, Ints}
import scorex.crypto.EllipticCurveImpl

import scala.collection.immutable.Stream

case class BlockCheckpoint(height: Int, signature: Array[Byte])

case class Checkpoint(items: Seq[BlockCheckpoint], signature: Array[Byte]) {
  lazy val toSign = {
    val length = items.size
    val lengthBytes = Ints.toByteArray(length)

    items.foldLeft(lengthBytes) { case (bs, BlockCheckpoint(h, s)) =>
      Bytes.concat(bs, Ints.toByteArray(h), s)
    }
  }

  def signedBy(privateKey: Array[Byte]) = copy(signature = EllipticCurveImpl.sign(privateKey, toSign))
}

object Checkpoint {
  def historyPoints(n: Int, maxRollback: Int, resultSize: Int = MaxCheckpoints): Seq[Int] =
    mult(maxRollback, 10).map(n - _).takeWhile(_ > 0).take(resultSize)

  private def mult(start: Int, step: Int): Stream[Int] =
    Stream.cons(start, mult(start * step, step))

  val MaxCheckpoints = 10
}
