package com.wavesplatform.lang.v1.task

import cats.mtl.MonadState
import cats.{Monad, MonadError, StackSafeMonad}
import com.wavesplatform.lang.EvalF

trait TaskMTInstances {

  object TF extends TaskMTFunctions

  implicit def monadError[F[_] : Monad, S, E](implicit m: Monad[EvalF[F, ?]]): MonadError[TaskMT[F, S, E, ?], E] =
    new MonadError[TaskMT[F, S, E, ?], E] with StackSafeMonad[TaskMT[F, S, E, ?]] {
      override def pure[A](x: A): TaskMT[F, S, E, A] =
        TF.pure(x)

      override def flatMap[A, B](fa: TaskMT[F, S, E, A])(f: A => TaskMT[F, S, E, B]): TaskMT[F, S, E, B] =
        fa.flatMap(f)

      override def raiseError[A](e: E): TaskMT[F, S, E, A] =
        TF.raiseError(e)

      override def handleErrorWith[A](fa: TaskMT[F, S, E, A])(f: E => TaskMT[F, S, E, A]): TaskMT[F, S, E, A] =
        fa.handleErrorWith(f)
    }

  implicit def monadState[F[_]: Monad, S, E](implicit m: Monad[EvalF[F, ?]]): MonadState[TaskMT[F, S, E, ?], S] =
    new MonadState[TaskMT[F, S, E, ?], S] {
      override val monad: Monad[TaskMT[F, S, E, ?]] = monadError[F, S, E]

      override def get: TaskMT[F, S, E, S] = TF.get

      override def set(s: S): TaskMT[F, S, E, Unit] = TF.set(s)

      override def inspect[A](f: S => A): TaskMT[F, S, E, A] = monad.map(get)(f)

      override def modify(f: S => S): TaskMT[F, S, E, Unit] = monad.flatMap(get)(f andThen set)
    }
}
