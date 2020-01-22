package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import cats.implicits._
import cats.{Eval, Monad}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.{ContextfulNativeFunction, ContextfulUserFunction}
import com.wavesplatform.lang.{EvalF, ExecutionError, TrampolinedExecResult}

import scala.annotation.meta.field
import scala.language.implicitConversions
import scala.scalajs.js.annotation._

sealed trait BaseFunction[C[_[_]]] {
  @JSExport def signature: FunctionTypeSignature
  @JSExport def header: FunctionHeader = signature.header
  def costByLibVersion: Map[StdLibVersion, Long]
  @JSExport def name: String
  @JSExport def args: Array[String]
  @JSExport def deprecated: Boolean = false
}

object BaseFunction {
  implicit def header[C[_[_]]](bf: BaseFunction[C]): FunctionHeader = bf.header
}

@JSExportTopLevel("FunctionTypeSignature")
case class FunctionTypeSignature(result: TYPE, args: Seq[(String, TYPE)], header: FunctionHeader)

@JSExportTopLevel("NativeFunction")
case class NativeFunction[C[_[_]]](
                                    @(JSExport @field) name: String,
                                    costByLibVersion: Map[StdLibVersion, Long],
                                    @(JSExport @field) signature: FunctionTypeSignature,
                                    ev: ContextfulNativeFunction[C],
                                    @(JSExport @field) args: Array[String]
) extends BaseFunction[C] {
  def eval[F[_] : Monad](context: C[F], args: List[EVALUATED]): TrampolinedExecResult[F, EVALUATED] =
    EitherT.apply[EvalF[F, ?], ExecutionError, EVALUATED](ev((context, args)).pure[Eval])
}

object NativeFunction {
  def withEnvironment[C[_[_]]](name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      ev: ContextfulNativeFunction[C]): NativeFunction[C] =
    new NativeFunction(
      name = name,
      costByLibVersion = DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev /*ev.orElse { case _ => "Passed argument with wrong type".asLeft[EVALUATED].pure[F] }(_, _)*/,
      args = args.map(_._1).toArray
    )

  def apply[C[_[_]]](name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
    ev: List[EVALUATED] => Either[ExecutionError, EVALUATED]
  ): NativeFunction[C] =
    withEnvironment[C](name, cost, internalName, resultType, args: _*)(new ContextfulNativeFunction[C] {
      override def apply[F[_]: Monad](a: (C[F], List[EVALUATED])): F[Either[ExecutionError, EVALUATED]] =
        ev(a._2).pure[F]
    })

  def apply[C[_[_]]](name: String,
            costByLibVersion: Map[StdLibVersion, Long],
            internalName: Short,
            resultType: TYPE,
            args: (String, TYPE, String)*)(ev: ContextfulNativeFunction[C]): NativeFunction[C] =
    new NativeFunction(
      name = name,
      costByLibVersion = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev,
      args = args.map(_._1).toArray
    )

}

@JSExportTopLevel("UserFunction")
case class UserFunction[C[_[_]]](
                                  @(JSExport@field) name: String,
                                  @(JSExport@field) internalName: String,
                                  costByLibVersion: Map[StdLibVersion, Long],
                                  @(JSExport@field) signature: FunctionTypeSignature,
                                  ev: ContextfulUserFunction[C],
                                  @(JSExport@field) args: Array[String]
) extends BaseFunction[C]

object UserFunction {
  def withEnvironment[C[_[_]]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: ContextfulUserFunction[C]): UserFunction[C] =
    UserFunction.withEnvironment(
      name = name,
      internalName = name,
      DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      resultType,
      args: _*
    )(ev)

  def apply[C[_[_]]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[C] =
    UserFunction.withEnvironment[C](name, cost, resultType, args: _ *)(new ContextfulUserFunction[C] {
      override def apply[F[_] : Monad](context: C[F]): EXPR = ev
    })

  def deprecated[C[_[_]]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[C] =
    UserFunction.deprecated(name, name, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args: _*)(ev)

  def apply[C[_[_]]](name: String, costByLibVersion: Map[StdLibVersion, Long], resultType: TYPE, args: (String, TYPE)*)(
      ev: EXPR): UserFunction[C] =
    UserFunction(name, name, costByLibVersion, resultType, args: _*)(ev)

  def apply[C[_[_]]](name: String, internalName: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(
      ev: EXPR): UserFunction[C] =
    UserFunction.withEnvironment[C](name, internalName, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args: _*)(
      ContextfulUserFunction.pure[C](ev)
    )

  def withEnvironment[C[_[_]]](
    name: String,
    internalName: String,
    costByLibVersion: Map[StdLibVersion, Long],
    resultType: TYPE,
    args: (String, TYPE)*
  )(ev: ContextfulUserFunction[C]): UserFunction[C] =
    new UserFunction(
      name = name,
      internalName = internalName,
      costByLibVersion = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ev = ev,
      args = args.map(_._1).toArray
    )

  def apply[C[_[_]]](
    name: String,
    internalName: String,
    costByLibVersion: Map[StdLibVersion, Long],
    resultType: TYPE,
    args: (String, TYPE)*
  )(ev: EXPR): UserFunction[C] =
    withEnvironment[C](name, internalName, costByLibVersion, resultType, args: _*)(
      ContextfulUserFunction.pure[C](ev)
    )

  def deprecated[C[_[_]]](
    name: String,
    internalName: String,
    costByLibVersion: Map[StdLibVersion, Long],
    resultType: TYPE,
    args: (String, TYPE)*
  )(ev: EXPR): UserFunction[C] =
    new UserFunction[C](
      name = name,
      internalName = internalName,
      costByLibVersion = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ContextfulUserFunction.pure[C](ev),
      args = args.map(_._1).toArray
    ) {
      override def deprecated = true
    }
}
