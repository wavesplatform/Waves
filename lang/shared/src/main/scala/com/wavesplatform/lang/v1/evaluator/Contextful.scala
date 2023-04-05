package com.wavesplatform.lang.v1.evaluator

import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.{Eval, Monad}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types.TYPE
import com.wavesplatform.lang.v1.traits.Environment

sealed trait ContextfulNativeFunction {
  val name: String
  val resultType: TYPE
  val args: Seq[(String, TYPE)]

  override def toString =
    s"""<$name(${args.map(a => s"${a._1}: ${a._2}").mkString(", ")}): $resultType>"""
}

object ContextfulNativeFunction {
  abstract class Simple(
      val name: String,
      val resultType: TYPE,
      val args: Seq[(String, TYPE)]
  ) extends ContextfulNativeFunction {
    def evaluate[F[_]: Monad](
        env: Environment[F],
        evaluatedArgs: List[EVALUATED]
    ): F[Either[ExecutionError, EVALUATED]]
  }
}

trait ContextfulUserFunction {
  def apply[F[_]: Monad](context: Environment[F], startArgs: List[EXPR]): EXPR
}

object ContextfulUserFunction {
  def pure(expr: EXPR): ContextfulUserFunction =
    new ContextfulUserFunction {
      override def apply[F[_]: Monad](context: Environment[F], startArgs: List[EXPR]): EXPR = expr
    }
}

trait ContextfulVal {
  val isPure: Boolean = false
  def apply[F[_]: Monad](context: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]]
}

object ContextfulVal {
  def fromEval(v: Eval[Either[ExecutionError, EVALUATED]]): ContextfulVal =
    new ContextfulVal {
      override def apply[F[_]: Monad](context: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        v.map(_.pure[F])
    }

  def pure(v: EVALUATED): ContextfulVal =
    new ContextfulVal {
      override val isPure: Boolean = true

      override def apply[F[_]: Monad](context: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
        v.asRight[ExecutionError].pure[F].pure[Eval]
    }

  trait Lifted extends ContextfulVal {
    override def apply[F[_]: Monad](context: Environment[F]): Eval[F[Either[ExecutionError, EVALUATED]]] =
      liftF(context).map(_.pure[F])

    def liftF[F[_]: Monad](context: Environment[F]): Eval[Either[ExecutionError, EVALUATED]]
  }
}
