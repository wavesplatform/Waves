package com.wavesplatform.transaction.smart.script.trace

import cats.implicits._
import cats.kernel.Semigroup
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.transaction.Transaction
import play.api.libs.json.{JsObject, Json}

final case class TracedResult[+E, +A](
    resultE: Either[E, A],
    trace: List[TraceStep] = Nil
) {
  def transformE[B, E1](f: Either[E, A] => TracedResult[E1, B]): TracedResult[E1, B] = {
    val newResultE = f(resultE)
    newResultE.copy(trace = this.trace ::: newResultE.trace)
  }

  def flatMap[B, E1 >: E](f: A => TracedResult[E1, B]): TracedResult[E1, B] = {
    resultE match {
      case Left(_) => this.asInstanceOf[TracedResult[E1, B]]
      case Right(value) =>
        val newResult = f(value)
        newResult.copy(trace = this.trace ::: newResult.trace)
    }
  }

  def map[B](f: A => B): TracedResult[E, B] = copy(resultE.map(f))

  def leftMap[E1](f: E => E1): TracedResult[E1, A] = copy(resultE.leftMap(f))

  def json(implicit ev1: E => ApiError, ev2: A => Transaction): JsObject = {
    val resultJson = resultE match {
      case Right(value) => value.json()
      case Left(e)      => e.json
    }
    resultJson ++ Json.obj("trace" -> trace.map(_.json))
  }
}

object TracedResult {
  implicit def wrapE[A, E](e: Either[E, A]): TracedResult[E, A] = TracedResult(e)

  implicit def wrapValue[A, E](value: A): TracedResult[E, A] = TracedResult(Right(value))

  implicit def tracedResultSemigroup[A: Semigroup, E]: Semigroup[TracedResult[E, A]] =
    (a, b) =>
      TracedResult(
        a.resultE |+| b.resultE,
        a.trace |+| b.trace
    )
}
