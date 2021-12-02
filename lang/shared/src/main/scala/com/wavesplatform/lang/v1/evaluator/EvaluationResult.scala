package com.wavesplatform.lang.v1.evaluator

import cats.{Monad, StackSafeMonad}
import cats.syntax.either._
import com.wavesplatform.lang.{ExecutionError, StringError}
import monix.eval.Coeval

case class EvaluationResult[A](value: Coeval[Either[(ExecutionError, Int), A]]) {
  def flatMap[B](f: A => EvaluationResult[B]): EvaluationResult[B] =
    EvaluationResult.monad.flatMap(this)(f)

  def map[B](f: A => B): EvaluationResult[B] =
    EvaluationResult(value.map(_.map(f)))
}

object EvaluationResult {
  def apply[A](value: => A): EvaluationResult[A]               = EvaluationResult(Coeval(Right(value)))
  def apply[A](error: String, limit: Int): EvaluationResult[A] = EvaluationResult(Coeval(Left((StringError(error), limit))))

  implicit val monad: Monad[EvaluationResult] = new StackSafeMonad[EvaluationResult] {
    override def pure[A](a: A): EvaluationResult[A] =
      EvaluationResult(a)

    override def flatMap[A, B](fa: EvaluationResult[A])(f: A => EvaluationResult[B]): EvaluationResult[B] =
      EvaluationResult(fa.value.flatMap {
        case l @ Left(_) => Coeval(l.rightCast[B])
        case Right(r)    => f(r).value
      })
  }
}
