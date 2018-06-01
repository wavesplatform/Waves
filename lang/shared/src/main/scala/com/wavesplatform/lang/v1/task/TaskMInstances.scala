package com.wavesplatform.lang.v1.task

import cats.data.Kleisli
import cats.mtl.MonadState
import cats.{Monad, MonadError}
import monix.eval.Coeval

trait TaskMInstances {

  object TF extends TaskMFunctions

  implicit def monadError[S, E]: MonadError[({ type λ[A] = TaskM[S, E, A] })#λ, E] = new MonadError[({ type λ[A] = TaskM[S, E, A] })#λ, E] {
    override def pure[A](x: A): TaskM[S, E, A] = TF.pure(x)

    override def flatMap[A, B](fa: TaskM[S, E, A])(f: A => TaskM[S, E, B]): TaskM[S, E, B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => TaskM[S, E, Either[A, B]]): TaskM[S, E, B] = {
      TaskM.fromKleisli(Monad[({ type λ[A] = Kleisli[Coeval, CoevalRef[S], A] })#λ].tailRecM(a)(f andThen (_.inner.map {
        case Left(err)        => Right(Left(err))
        case Right(Left(lv))  => Left(lv)
        case Right(Right(rv)) => Right(Right(rv))
      })))
    }

    override def raiseError[A](e: E): TaskM[S, E, A] = TF.raiseError(e)

    override def handleErrorWith[A](fa: TaskM[S, E, A])(f: E => TaskM[S, E, A]): TaskM[S, E, A] = fa.handleErrorWith(f)
  }

  implicit def monadState[S, E]: MonadState[({ type λ[A] = TaskM[S, E, A] })#λ, S] = new MonadState[({ type λ[A] = TaskM[S, E, A] })#λ, S] {
    override val monad: Monad[({ type λ[A] = TaskM[S, E, A] })#λ] = monadError[S, E]

    override def get: TaskM[S, E, S] = TF.get

    override def set(s: S): TaskM[S, E, Unit] = TF.set(s)

    override def inspect[A](f: S => A): TaskM[S, E, A] = monad.map(get)(f)

    override def modify(f: S => S): TaskM[S, E, Unit] = monad.flatMap(get)(f andThen set)
  }
}
