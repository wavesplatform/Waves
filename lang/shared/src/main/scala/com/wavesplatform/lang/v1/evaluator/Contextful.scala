package com.wavesplatform.lang.v1.evaluator

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.{Eval, Monad}
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types.TYPE
import com.wavesplatform.lang.{CoevalF, ExecutionError}
import monix.eval.Coeval

sealed trait ContextfulNativeFunction[C[_[_]]] {
  val name: String
  val resultType: TYPE
  val args: Seq[(String, TYPE)]

  override def toString =
    s"""<$name(${args.map(a => s"${a._1}: ${a._2}").mkString(", ")}): $resultType>"""
}

object ContextfulNativeFunction {
  abstract class Simple[C[_[_]]](
      val name: String,
      val resultType: TYPE,
      val args: Seq[(String, TYPE)]
  ) extends ContextfulNativeFunction[C] {
    def evaluate[F[_]: Monad](
        env: C[F],
        evaluatedArgs: List[EVALUATED]
    ): F[Either[ExecutionError, EVALUATED]]
  }

  abstract class Extended[C[_[_]]](
      val name: String,
      val resultType: TYPE,
      val args: Seq[(String, TYPE)]
  ) extends ContextfulNativeFunction[C] {
    def evaluate[F[_]: Monad](
        env: C[F],
        evaluatedArgs: List[EVALUATED],
        availableComplexity: Int
    )(implicit m: Monad[CoevalF[F, *]]): Coeval[F[(Either[ExecutionError, (EVALUATED, Log[F])], Int)]]
  }
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
