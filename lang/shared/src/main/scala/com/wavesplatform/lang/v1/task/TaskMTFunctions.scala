package com.wavesplatform.lang.v1.task

import cats.{Eval, Monad}
import cats.data.Kleisli
import cats.implicits._

trait TaskMTFunctions {

  def pure[F[_] : Monad, S, E, R](x: R): TaskMT[F, S, E, R] =
    TaskMT(_ => Eval.now(x.asRight.pure))

  def raiseError[F[_] : Monad, S, E, R](e: E): TaskMT[F, S, E, R] =
    TaskMT(_ => Eval.now(e.asLeft.pure))

  def liftEither[F[_] : Monad, S, E, R](ei: Either[E, R]): TaskMT[F, S, E, R] =
    TaskMT.fromKleisli(Kleisli.pure(ei.pure))

  def get[F[_] : Monad, S, E]: TaskMT[F, S, E, S] =
    TaskMT(s => Eval.now(s.asRight.pure))

  def set[F[_] : Monad, S, E](s: S): TaskMT[F, S, E, Unit] =
    TaskMT.fromKleisli(Kleisli(ref => {
      ref.write(s).map(_.asRight.pure)
    }))

  def local[F[_], S, E, A](fa: TaskMT[F, S, E, A]): TaskMT[F, S, E, A] = {
    TaskMT.fromKleisli(Kleisli((ref: EvalRef[S]) => {
      val newRef = ref.copy()
      fa.inner.run(newRef)
    }))
  }

  def id[F[_], S, E, A](fa: TaskMT[F, S, E, A]): TaskMT[F, S, E, A] = fa

  def inspect[F[_] : Monad, S, E, A](f: S => A): TaskMT[F, S, E, A] =
    get[F, S, E].map(f)

  def inspectFlat[F[_] : Monad, S, E, A](f: S => TaskMT[F, S, E, A]): TaskMT[F, S, E, A] =
    get[F, S, E].flatMap(f)

  def modify[F[_] : Monad, S, E](f: S => S): TaskMT[F, S, E, Unit] =
    get[F, S, E].flatMap(f andThen set)
}
