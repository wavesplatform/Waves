package com.wavesplatform

import cats.{Eval, Id, Monad, StackSafeMonad}
import cats.data.EitherT
import monix.eval.Coeval

package object lang {
  type ExecutionError = String
  type ExecutionLog   = String

  type CoevalF[F[_], A]               = Coeval[F[A]]
  type EvalF[F[_], A]                 = Eval[F[A]]
  type TrampolinedExecResult[F[_], T] = EitherT[EvalF[F, *], ExecutionError, T]

  implicit val idCoevalFMonad: Monad[CoevalF[Id, *]] = new StackSafeMonad[CoevalF[Id, *]] {
    override def flatMap[A, B](fa: Coeval[A])(f: A => Coeval[B]): Coeval[B] =
      fa.flatMap(f).memoize

    override def pure[A](x: A): Coeval[A] =
      Coeval.now(x)
  }
}
