package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import cats.{Eval, Monad}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types.TYPE
import monix.eval.Coeval

abstract class ContextfulNativeFunction[C[_[_]]](name: String, resultType: TYPE, args: Seq[(String, TYPE)]) {
  def ev[F[_]: Monad](input: (C[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]]

  final def apply[F[_]: Monad](input: (C[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] = {
    try {
      ev(input)
    } catch {
      case _: SecurityException =>
        Either
          .left[ExecutionError, EVALUATED](
            s"""An access to <$name(${args.toSeq.map(a => s"${a._1}: ${a._2}").mkString(", ")}): $resultType> is denied"""
          )
          .pure[F]
      case e: Throwable =>
        Either
          .left[ExecutionError, EVALUATED](s"""An error during run <$name(${args.toSeq
            .map(a => s"${a._1}: ${a._2}")
            .mkString(", ")}): $resultType>: ${e.getClass()} ${e.getMessage() match {
            case null => e.toString
            case msg  => msg
          }}""")
          .pure[F]
    }
  }

  def evaluateExtended[F[_]: Monad](env: C[F], args: List[EVALUATED], availableComplexity: Int): Coeval[F[(Either[ExecutionError, EVALUATED], Int)]] =
    Coeval.now(apply((env, args)).map((_, 0)))
}

trait ContextfulUserFunction[C[_[_]]] {
  def apply[F[_]: Monad](context: C[F], startArgs: List[EXPR]): EXPR
}

object ContextfulUserFunction {
  def pure[C[_[_]]](expr: EXPR): ContextfulUserFunction[C] =
    new ContextfulUserFunction[C] {
      override def apply[F[_]: Monad](context: C[F], startArgs: List[EXPR]): EXPR = expr
    }
}

trait ContextfulVal[C[_[_]]] {
  val isPure: Boolean = false
  def apply[F[_]: Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]]
}

object ContextfulVal {
  def fromEval[C[_[_]]](v: Eval[Either[ExecutionError, EVALUATED]]): ContextfulVal[C] =
    new ContextfulVal[C] {
      override def apply[F[_]: Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        v.map(_.pure[F])
    }

  def pure[C[_[_]]](v: EVALUATED): ContextfulVal[C] =
    new ContextfulVal[C] {
      override val isPure: Boolean = true

      override def apply[F[_]: Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        v.asRight[ExecutionError].pure[F].pure[Eval]
    }

  trait Lifted[C[_[_]]] extends ContextfulVal[C] {
    override def apply[F[_]: Monad](context: C[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
      liftF(context).map(_.pure[F])

    def liftF[F[_]: Monad](context: C[F]): Eval[Either[ExecutionError, EVALUATED]]
  }
}

object Contextful {
  type NoContext[_[_]] = Any
  def empty[F[_]]: NoContext[F] = ()
}
