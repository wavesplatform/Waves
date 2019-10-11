package com.wavesplatform.lang.v1.repl

import cats.implicits._
import cats.{Eval, Monad, StackSafeMonad}
import com.wavesplatform.lang.EvalF

import scala.concurrent.ExecutionContext.Implicits.{global => g}
import scala.concurrent.Future

object Implicits {
  implicit def stackUnsafeMonad[F[_] : Monad]: Monad[EvalF[F, ?]] =
    new StackSafeMonad[EvalF[F, ?]] {
      override def flatMap[A, B](fa: Eval[F[A]])(f: A => Eval[F[B]]): Eval[F[B]] =
        fa.map(_.flatMap(f(_).value))

      override def pure[A](x: A): Eval[F[A]] =
        x.pure[F].pure[Eval]
    }

  implicit val futureMonad: Monad[Future] = Monad[Future]
  implicit val futureEvalFMonad: Monad[EvalF[Future, ?]] = stackUnsafeMonad[Future]
}
