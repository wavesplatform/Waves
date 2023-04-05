package com.wavesplatform.lang.v1.evaluator

import cats.syntax.either.*
import cats.{Monad, StackSafeMonad}
import com.wavesplatform.lang.{CommonError, ExecutionError}
import monix.eval.Coeval

case class EvaluationResult[+A](value: Coeval[Either[(ExecutionError, Int), A]]) {
  def flatMap[B](f: A => EvaluationResult[B]): EvaluationResult[B] =
    EvaluationResult.monad.flatMap(this)(f)

  def map[B](f: A => B): EvaluationResult[B] =
    EvaluationResult(value.map(_.map(f)))
}

object EvaluationResult {
  def apply[A](value: A): EvaluationResult[A]                  = EvaluationResult(Coeval.now(Right(value)))
  def apply[A](error: String, limit: Int): EvaluationResult[A] = EvaluationResult(Coeval.now(Left((CommonError(error), limit))))

  implicit val monad: Monad[EvaluationResult] = new StackSafeMonad[EvaluationResult] {
    override def pure[A](a: A): EvaluationResult[A] =
      EvaluationResult(a)

    override def flatMap[A, B](fa: EvaluationResult[A])(f: A => EvaluationResult[B]): EvaluationResult[B] =
      EvaluationResult(fa.value.flatMap {
        case l @ Left(_) => Coeval.now(l.rightCast[B])
        case Right(r)    => f(r).value
      })
  }
}

object Defer {
  def apply[A](r: => EvaluationResult[A]): EvaluationResult[A] =
    EvaluationResult(Coeval.defer(r.value))
}
