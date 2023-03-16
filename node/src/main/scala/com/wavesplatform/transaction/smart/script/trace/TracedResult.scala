package com.wavesplatform.transaction.smart.script.trace

import cats.instances.either.*
import cats.instances.list.*
import cats.kernel.Semigroup
import cats.syntax.either.*
import cats.syntax.semigroup.*
import cats.{Monad, StackSafeMonad}

final case class TracedResult[+E, +A](
    resultE: Either[E, A],
    trace: List[TraceStep] = Nil,
    attributes: TracedResult.Attributes = Map.empty
) {
  def flatMap[B, E1 >: E](f: A => TracedResult[E1, B]): TracedResult[E1, B] = {
    resultE match {
      case Left(_) => this.asInstanceOf[TracedResult[E1, B]]
      case Right(value) =>
        val newResult = f(value)
        newResult.copy(trace = this.trace ::: newResult.trace, attributes = this.attributes ++ newResult.attributes)
    }
  }

  def map[B](f: A => B): TracedResult[E, B] = copy(resultE.map(f))

  def leftMap[E1](f: E => E1): TracedResult[E1, A] = copy(resultE.leftMap(f))

  // added for for-comprehension destructuring
  def withFilter(f: A => Boolean): TracedResult[E, A] =
    copy(resultE.filterOrElse(f, throw new MatchError("TracedResult destructuring error")))

  def attribute[T](key: TracedResult.Attribute): T =
    attributes(key).asInstanceOf[T]

  def attributeOpt[T](key: TracedResult.Attribute): Option[T] =
    attributes.get(key).asInstanceOf[Option[T]]

  def withAttributes(as: (TracedResult.Attribute, Any)*): TracedResult[E, A] =
    copy(attributes = this.attributes ++ as)
}

object TracedResult {
  trait Attribute
  object Attribute {
    case object MinFee extends Attribute
  }

  type Attributes = Map[TracedResult.Attribute, Any]

  implicit def wrapE[A, E](e: Either[E, A]): TracedResult[E, A] = TracedResult(e)

  def wrapValue[A, E](value: A): TracedResult[E, A] = TracedResult(Right(value))

  implicit def tracedResultSemigroup[A: Semigroup, E]: Semigroup[TracedResult[E, A]] =
    (a, b) =>
      TracedResult(
        a.resultE |+| b.resultE,
        a.trace |+| b.trace
      )

  implicit def monadTracedResult[L]: Monad[TracedResult[L, *]] =
    new StackSafeMonad[TracedResult[L, *]] {
      override def flatMap[A, B](fa: TracedResult[L, A])(f: A => TracedResult[L, B]): TracedResult[L, B] = fa.flatMap(f)
      override def pure[A](x: A): TracedResult[L, A]                                                     = TracedResult.wrapValue(x)
    }
}
