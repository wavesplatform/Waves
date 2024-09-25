package com.wavesplatform.lang.v1.repl

import cats.implicits.*
import cats.{Eval, Monad, StackSafeMonad}
import com.wavesplatform.lang.{CoevalF, EvalF}
import monix.eval.Coeval

object Implicits {
  implicit def stackUnsafeMonad[F[_]: Monad]: Monad[EvalF[F, *]] =
    new StackSafeMonad[EvalF[F, *]] {
      override def flatMap[A, B](fa: Eval[F[A]])(f: A => Eval[F[B]]): Eval[F[B]] =
        fa.map(_.flatMap(f(_).value))

      override def pure[A](x: A): Eval[F[A]] =
        x.pure[F].pure[Eval]
    }

  implicit def stackUnsafeMonadCoeval[F[_]: Monad]: Monad[CoevalF[F, *]] =
    new StackSafeMonad[CoevalF[F, *]] {
      override def flatMap[A, B](fa: Coeval[F[A]])(f: A => Coeval[F[B]]): Coeval[F[B]] =
        fa.map(_.flatMap(f(_).value()))

      override def pure[A](x: A): Coeval[F[A]] =
        x.pure[F].pure[Coeval]
    }
}
