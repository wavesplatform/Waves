package com.wavesplatform

import cats.Monoid
import cats.data.{NonEmptyList => NEL}
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{Transaction, ValidationError}

import scala.annotation.tailrec
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

  def splitAfterThreshold[A](list: NEL[A], threshold: Int)(count: A => Int): (NEL[A], List[A]) = {
    @tailrec
    def r(agg: NEL[A], aggCount: Int, rest: List[A]): (NEL[A], List[A]) =
      if (aggCount >= threshold) (agg, rest)
      else rest match {
        case Nil => (agg, rest)
        case (x :: xs) => r(x :: agg, count(x) + aggCount, xs)
      }

    r(NEL.one(list.head), count(list.head), list.tail)
  }

  def perpendCompact[A](`new`: A, existing: NEL[A])(compactPred: (A, A) => Boolean)(implicit ma: Monoid[A]): NEL[A] = {
    if (compactPred(`new`, existing.head)) NEL(Monoid.combine(existing.head, `new`), existing.tail)
    else `new` :: existing
  }

  def perpendCompact[A](`new`: A, existing: List[A])(compactPred: (A, A) => Boolean)(implicit ma: Monoid[A]): NEL[A] = existing match {
    case (x :: xs) => perpendCompact(`new`, NEL(x, xs))(compactPred)
    case Nil => NEL.one(`new`)
  }

  def prependCompactBlockDiff(`new`: BlockDiff, existing: NEL[BlockDiff], maxTxsInChunk: Int): NEL[BlockDiff] =
    perpendCompact[BlockDiff](`new`, existing) { case (x, y) => x.txsDiff.transactions.size + y.txsDiff.transactions.size <= maxTxsInChunk }


  def sameQuotient(x: Int, y: Int, divisor: Int): Boolean = (x / divisor) == (y / divisor)
}
