package com.wavesplatform.lang.v1.task

import cats.data.Kleisli
import cats.implicits._
import cats.{Eval, Functor, Monad}
import com.wavesplatform.lang.EvalF
import monix.execution.atomic.{Atomic, AtomicBuilder}

/**
  * Monad with ability to handle errors and deal with stateful computations
  *
  * @tparam S - State type
  * @tparam E - Error type
  * @tparam R - Result type
  * @tparam F - Result context type
  */
trait TaskMT[F[_], S, E, R] {
  protected[task] val inner: Kleisli[Eval, EvalRef[S], F[Either[E, R]]]

  def run[RS <: Atomic[S]](initial: S)(implicit b: AtomicBuilder[S, RS]): Eval[(S, F[Either[E, R]])] = {
    val stateRef = EvalRef.of(initial)

    for {
      result     <- inner.run(stateRef)
      finalState <- stateRef.read
    } yield (finalState, result)
  }

  def map[B](f: R => B)(implicit ev: Functor[F]): TaskMT[F, S, E, B] =
    TaskMT.fromKleisli(inner.map(_.map {
      case Right(v)  => Right(f(v))
      case Left(err) => Left(err)
    }))

  def flatMap[B](f: R => TaskMT[F, S, E, B])(implicit m: Monad[EvalF[F, ?]]): TaskMT[F, S, E, B] = {
    TaskMT.fromEvalRef[F, S, E, B] { s =>
      m.flatMap(inner.run(s)) {
        case Right(v)  => f(v).inner.run(s)
        case Left(err) => m.pure(err.asLeft[B])
      }
    }
  }

  def handleErrorWith(f: E => TaskMT[F, S, E, R])(implicit m: Monad[EvalF[F, ?]]): TaskMT[F, S, E, R] =
    TaskMT.fromEvalRef[F, S, E, R] { s =>
      m.flatMap(inner.run(s)) {
        case Right(v)  => m.pure(v.asRight[E])
        case Left(err) => f(err).inner.run(s)
      }
    }
}

object TaskMT {
  private[task] def fromKleisli[F[_], S, E, R](in: Kleisli[Eval, EvalRef[S], F[Either[E, R]]]): TaskMT[F, S, E, R] =
    new TaskMT[F, S, E, R] {
      override protected[task] val inner: Kleisli[Eval, EvalRef[S], F[Either[E, R]]] = in
    }

  def apply[F[_], S, E, R](f: S => Eval[F[Either[E, R]]]): TaskMT[F, S, E, R] =
    fromEvalRef(_.read flatMap f)

  private def fromEvalRef[F[_], S, E, R](f: EvalRef[S] => Eval[F[Either[E, R]]]): TaskMT[F, S, E, R] =
    new TaskMT[F, S, E, R] {
      override protected[task] val inner: Kleisli[Eval, EvalRef[S], F[Either[E, R]]] = Kleisli(f)
    }
}
