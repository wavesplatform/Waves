package com.wavesplatform.lang.v1.task

import cats.data.Kleisli
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.{Eval, Monad}
import com.wavesplatform.lang.EvalF

trait TaskMTFunctions {

  def pure[F[_]: Monad, S, E, R](x: R): TaskMT[F, S, E, R] =
    TaskMT(_ => Eval.now(x.asRight[E].pure[F]))

  def raiseError[F[_]: Monad, S, E, R](e: E): TaskMT[F, S, E, R] =
    TaskMT(_ => Eval.now(e.asLeft[R].pure[F]))

  def liftEither[F[_]: Monad, S, E, R](ei: Either[E, R]): TaskMT[F, S, E, R] =
    TaskMT.fromKleisli(Kleisli.pure(ei.pure[F]))

  def get[F[_]: Monad, S, E]: TaskMT[F, S, E, S] =
    TaskMT(s => Eval.now(s.asRight[E].pure[F]))

  def set[F[_]: Monad, S, E](s: S): TaskMT[F, S, E, Unit] =
    TaskMT.fromKleisli(Kleisli(ref => {
      ref.write(s).map(_.asRight[E].pure[F])
    }))

  def local[F[_], S, E, A](fa: TaskMT[F, S, E, A]): TaskMT[F, S, E, A] = {
    TaskMT.fromKleisli(Kleisli((ref: EvalRef[S]) => {
      val newRef = ref.copy()
      fa.inner.run(newRef)
    }))
  }

  def id[F[_], S, E, A](fa: TaskMT[F, S, E, A]): TaskMT[F, S, E, A] = fa

  def inspect[F[_]: Monad, S, E, A](f: S => A): TaskMT[F, S, E, A] =
    get[F, S, E].map(f)

  def inspectFlat[F[_]: Monad, S, E, A](f: S => TaskMT[F, S, E, A])(implicit m: Monad[EvalF[F, *]]): TaskMT[F, S, E, A] =
    get[F, S, E].flatMap(f)

  def modify[F[_]: Monad, S, E](f: S => S)(implicit m: Monad[EvalF[F, *]]): TaskMT[F, S, E, Unit] =
    get[F, S, E].flatMap(f andThen set[F, S, E])
}
