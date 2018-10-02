package com.wavesplatform.lang.v1.task

import cats.data.Kleisli
import monix.eval.Coeval
import cats.implicits._

trait TaskMFunctions {

  def pure[S, E, R](x: R): TaskM[S, E, R] = TaskM(_ => Coeval.pure(x.asRight))

  def raiseError[S, E, R](e: E): TaskM[S, E, R] = TaskM(_ => Coeval.pure(e.asLeft))

  def liftEither[S, E, R](ei: Either[E, R]): TaskM[S, E, R] = TaskM.fromKleisli(Kleisli.pure(ei))

  def get[S, E]: TaskM[S, E, S] = TaskM(s => Coeval.pure(s.asRight))

  def set[S, E](s: S): TaskM[S, E, Unit] =
    TaskM.fromKleisli(Kleisli(ref => {
      ref.write(s).map(_.asRight)
    }))

  def local[S, E, A](fa: TaskM[S, E, A]): TaskM[S, E, A] = {
    TaskM.fromKleisli(Kleisli((ref: CoevalRef[S]) => {
      val newRef = ref.copy()
      fa.inner.run(newRef)
    }))
  }

  def id[S, E, A](fa: TaskM[S, E, A]): TaskM[S, E, A] = fa

  def inspect[S, E, A](f: S => A): TaskM[S, E, A]                  = get[S, E].map(f)

  def inspectFlat[S, E, A](f: S => TaskM[S, E, A]): TaskM[S, E, A] = get[S, E].flatMap(f)

  def modify[S, E](f: S => S): TaskM[S, E, Unit] = get[S, E].flatMap(f andThen set)
}
