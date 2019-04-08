package com.wavesplatform.state.diffs

import cats.kernel.Semigroup
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.smart.script.TraceStep
import cats.implicits._

case class TracedResult[+A](
  result: Either[ValidationError, A],
  trace:  List[TraceStep] = Nil
) {
  def flatMap[B](f: A => TracedResult[B]): TracedResult[B] = {
    result match {
      case Left(_)      => this.asInstanceOf[TracedResult[B]]
      case Right(value) =>
        val newResult = f(value)
        newResult.copy(trace = this.trace ::: newResult.trace)
    }
  }

  def map[B](f: A => B): TracedResult[B] = copy(result.map(f))

  def leftMap(f: ValidationError => ValidationError): TracedResult[A] = copy(result.leftMap(f))
}

object TracedResult {
  implicit def wrapE[A](e: Either[ValidationError, A]): TracedResult[A] = TracedResult(e)
  implicit def wrapValue[A](value: A): TracedResult[A] = TracedResult(Right(value))

  implicit def tracedResultSemigroup[A : Semigroup]: Semigroup[TracedResult[A]] = (a, b) =>
    TracedResult(
      a.result |+| b.result,
      a.trace  |+| b.trace
  )
}
