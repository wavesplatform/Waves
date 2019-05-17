package com.wavesplatform.lang.v1.task

import cats.data.Kleisli
import cats.mtl.MonadState
import cats.{Eval, Monad, MonadError}

trait TaskMInstances {

  object TF extends TaskMFunctions

  implicit def monadError[S, E]: MonadError[TaskM[S, E, ?], E] = new MonadError[TaskM[S, E, ?], E] {
    override def pure[A](x: A): TaskM[S, E, A] = TF.pure(x)

    override def flatMap[A, B](fa: TaskM[S, E, A])(f: A => TaskM[S, E, B]): TaskM[S, E, B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => TaskM[S, E, Either[A, B]]): TaskM[S, E, B] = {
      TaskM.fromKleisli(Monad[Kleisli[Eval, EvalRef[S], ?]].tailRecM(a)(f andThen (_.inner.map {
        case Left(err)        => Right(Left(err))
        case Right(Left(lv))  => Left(lv)
        case Right(Right(rv)) => Right(Right(rv))
      })))
    }

    override def raiseError[A](e: E): TaskM[S, E, A] = TF.raiseError(e)

    override def handleErrorWith[A](fa: TaskM[S, E, A])(f: E => TaskM[S, E, A]): TaskM[S, E, A] = fa.handleErrorWith(f)
  }

  implicit def monadState[S, E]: MonadState[TaskM[S, E, ?], S] = new MonadState[TaskM[S, E, ?], S] {
    override val monad: Monad[TaskM[S, E, ?]] = monadError[S, E]

    override def get: TaskM[S, E, S] = TF.get

    override def set(s: S): TaskM[S, E, Unit] = TF.set(s)

    override def inspect[A](f: S => A): TaskM[S, E, A] = monad.map(get)(f)

    override def modify(f: S => S): TaskM[S, E, Unit] = monad.flatMap(get)(f andThen set)
  }
}
