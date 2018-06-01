package com.wavesplatform.lang.v1.task

import cats.data.Kleisli
import cats.implicits._
import monix.eval.Coeval
import monix.execution.atomic.{Atomic, AtomicBuilder}

/**
  * Monad with ability to handle errors and deal with stateful computations
  *
  * @tparam S - State type
  * @tparam E - Error type
  * @tparam R - Result type
  */
trait TaskM[S, E, R] {
  protected[task] val inner: Kleisli[Coeval, CoevalRef[S], Either[E, R]]

  def run[RS <: Atomic[S]](initial: S)(implicit b: AtomicBuilder[S, RS]): Coeval[(S, Either[E, R])] = {
    val stateRef = CoevalRef.of(initial)

    for {
      result     <- inner.run(stateRef)
      finalState <- stateRef.read
    } yield (finalState, result)
  }

  def map[B](f: R => B): TaskM[S, E, B] =
    TaskM.fromKleisli(inner.map({
      case Right(v)  => Right(f(v))
      case Left(err) => Left(err)
    }))

  def flatMap[B](f: R => TaskM[S, E, B]): TaskM[S, E, B] = {
    TaskM.fromKleisli(inner.flatMap({
      case Right(v)  => f(v).inner
      case Left(err) => Kleisli.pure(err.asLeft[B])
    }))
  }

  def handleErrorWith(f: E => TaskM[S, E, R]): TaskM[S, E, R] = {
    TaskM.fromKleisli(inner.flatMap({
      case Right(v)  => Kleisli.pure(v.asRight)
      case Left(err) => f(err).inner
    }))
  }
}

object TaskM {

  private[task] def fromKleisli[S, E, R](in: Kleisli[Coeval, CoevalRef[S], Either[E, R]]): TaskM[S, E, R] = new TaskM[S, E, R] {
    override protected[task] val inner: Kleisli[Coeval, CoevalRef[S], Either[E, R]] = in
  }

  def apply[S, E, R](f: S => Coeval[Either[E, R]]): TaskM[S, E, R] = new TaskM[S, E, R] {
    override protected[task] val inner: Kleisli[Coeval, CoevalRef[S], Either[E, R]] = Kleisli(_.read >>= f)
  }
}
