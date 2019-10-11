package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import cats.{Eval, Monad}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}

trait Contextful[C[_[_]]] {
  def apply[F[_]: Monad](input: (C[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]]
}

trait UserContextfulFunction[C[_[_]]] {
  def apply[F[_]: Monad](context: C[F]): EXPR
}

object UserContextfulFunction {
  def pure[C[_[_]]](expr: EXPR): UserContextfulFunction[C] =
    new UserContextfulFunction[C] {
      override def apply[F[_] : Monad](context: C[F]): EXPR = expr
    }
}

trait ContextfulVal[C[_[_]]] {
  def apply[F[_]: Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]]
}

object ContextfulVal {
  def fromEval[C[_[_]]](v: Eval[Either[ExecutionError, EVALUATED]]): ContextfulVal[C] =
    new ContextfulVal[C] {
      override def apply[F[_] : Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        v.map(_.pure[F])
    }

  def pure[C[_[_]]](v: EVALUATED): ContextfulVal[C] =
    new ContextfulVal[C] {
      override def apply[F[_] : Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        v.asRight[ExecutionError].pure[F].pure[Eval]
    }

  trait Lifted[C[_[_]]] extends ContextfulVal[C] {
    override def apply[F[_] : Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
      liftF(context).map(_.pure[F])

    def liftF[F[_]: Monad](context: C[F]): Eval[Either[ExecutionError, EVALUATED]]
  }
}

object Contextful {
  type NoContext[_[_]] = Nothing
  def dummy[F[_]]: NoContext[F] = ???
}

