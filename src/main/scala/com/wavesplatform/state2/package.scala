package com.wavesplatform

import cats.Monoid
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{Transaction, ValidationError}

import scala.util.{Left, Right, Try}

package object state2 {

  type StateReader = Coeval[SnapshotStateReader]

  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  implicit class EitherExt[L <: ValidationError, R](ei: Either[L, R]) {
    def liftValidationError[T <: Transaction](t: T): Either[ValidationError, R] = {
      ei.left.map(e => GenericError(e.toString))
    }
  }

  implicit class EitherExt2[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value) => throw new Exception(value.toString)
      case Right(value) => value
    }
  }

  def ranges(from: Int, to: Int, by: Int): Stream[(Int, Int)] =
    if (from + by < to)
      (from, from + by) #:: ranges(from + by, to, by)
    else
      (from, to) #:: Stream.empty[(Int, Int)]

  def dropLeftIf[A](list: List[A])(cond: List[A] => Boolean): List[A] = list match {
    case l@(x :: xs) => if (cond(l)) dropLeftIf(xs)(cond) else l
    case Nil => Nil
  }

  def splitAfterThreshold[A](list: List[A])(base: Int, count: A => Int, threshold: Int): (List[A], List[A]) = {
    val splitIdx = list.zipWithIndex.foldLeft((base, Option.empty[Int])) { case ((collectedValue, maybeIdx), (item, idx)) =>
      maybeIdx match {
        case Some(_) => (collectedValue, maybeIdx)
        case None =>
          val tot = collectedValue + count(item)
          if (tot >= threshold) (tot, Some(idx + 1)) else (tot, None)
      }
    }._2.getOrElse(Int.MaxValue)
    list.splitAt(splitIdx)
  }

  def perpendCompact[A](`new`: A, existing: Seq[A])(compactPred: (A, A) => Boolean)(implicit ma: Monoid[A]): Seq[A] = existing match {
    case h0 :: tail =>
      if (compactPred(`new`, h0)) Monoid.combine(h0, `new`) +: tail
      else `new` +: existing
    case Nil => Seq(`new`)
  }

  def sameQuotient(x: Int, y: Int, divisor: Int): Boolean = (x / divisor) == (y / divisor)
}
