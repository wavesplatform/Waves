package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import cats.implicits._
import cats.{Eval, Monad, ~>}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.{EvalF, ExecutionError, TrampolinedExecResult}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.NativeFunction.{Input, Output}
import com.wavesplatform.lang.v1.traits.Environment

import scala.annotation.meta.field
import scala.scalajs.js.annotation._

sealed trait BaseFunction[C[_[_]]] {
  @JSExport def signature: FunctionTypeSignature
  @JSExport def header: FunctionHeader = signature.header
  def costByLibVersion: Map[StdLibVersion, Long]
  @JSExport def name: String
  @JSExport def args: Array[String]
  @JSExport def deprecated: Boolean = false

  def mapK[G[_]](f: F ~> G): BaseFunction[G, C]
}

object BaseFunction {
  implicit def header[F[_], C](bf: BaseFunction[F, C]): FunctionHeader = bf.header
}

@JSExportTopLevel("FunctionTypeSignature")
case class FunctionTypeSignature(result: TYPE, args: Seq[(String, TYPE)], header: FunctionHeader)

@JSExportTopLevel("NativeFunction")
case class NativeFunction[C[_[_]]](
  @(JSExport @field) name: String,
  costByLibVersion: Map[StdLibVersion, Long],
  @(JSExport @field) signature: FunctionTypeSignature,
  ev: Input[C, ?] ~> Output,
  @(JSExport @field) args: Array[String]
) extends BaseFunction[C] {
  def eval[F[_]](context: C[F], args: List[EVALUATED]): TrampolinedExecResult[F, EVALUATED] =
    EitherT.apply[EvalF[F, ?], ExecutionError, EVALUATED](ev((context, args)).pure[Eval])

  override def mapK[G[_]](f: F ~> G): BaseFunction[G, C] =
    copy(ev = (ctx, args) => f(ev(ctx, args)))
}

object NativeFunction {
  type Input[C[_[_]], F[_]]  = (C[F], List[EVALUATED])
  type Output[F[_]] = F[Either[ExecutionError, EVALUATED]]

  def withEnvironment[F[_] : Monad, C](name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      ev: PartialFunction[(C, List[EVALUATED]), F[Either[ExecutionError, EVALUATED]]]): NativeFunction[F, C] =
    new NativeFunction(
      name = name,
      costByLibVersion = DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev.orElse { case _ => "Passed argument with wrong type".asLeft[EVALUATED].pure[F] }(_, _),
      args = args.map(_._1).toArray
    )

  def apply[F[_] : Monad](
    name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*
  )(ev: PartialFunction[List[EVALUATED], F[Either[ExecutionError, EVALUATED]]]): NativeFunction[F, Nothing] =
    withEnvironment(name, cost, internalName, resultType, args: _*) { case (_, args) => ev(args) }

  def apply[F[_] : Monad, C](name: String,
            costByLibVersion: Map[StdLibVersion, Long],
            internalName: Short,
            resultType: TYPE,
            args: (String, TYPE, String)*)(ev: (C, List[EVALUATED]) => F[Either[ExecutionError, EVALUATED]]): NativeFunction[F, C] =
    new NativeFunction(
      name = name,
      costByLibVersion = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev,
      args = args.map(_._1).toArray
    )

}

@JSExportTopLevel("UserFunction")
case class UserFunction[F[_]](
  @(JSExport@field) name: String,
  @(JSExport@field) internalName: String,
  costByLibVersion: Map[StdLibVersion, Long],
  @(JSExport@field) signature: FunctionTypeSignature,
  ev: EXPR,
  @(JSExport@field) args: Array[String]
) extends BaseFunction[F, Nothing] {

  override def mapK[G[_]](f: F ~> G): BaseFunction[G, Nothing] =
    asInstanceOf[UserFunction[G]]
}

object UserFunction {

  def apply[F[_]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[F] =
    UserFunction(name, name, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args: _*)(ev)

  def deprecated[F[_]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[F] =
    UserFunction.deprecated(name, name, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args: _*)(ev)

  def apply[F[_]](name: String, costByLibVersion: Map[StdLibVersion, Long], resultType: TYPE, args: (String, TYPE)*)(
      ev: EXPR): UserFunction[F] =
    UserFunction(name, name, costByLibVersion, resultType, args: _*)(ev)

  def apply[F[_]](name: String, internalName: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(
      ev: EXPR): UserFunction[F] =
    UserFunction(name, internalName, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args: _*)(ev)

  def apply[F[_]](name: String,
            internalName: String,
            costByLibVersion: Map[StdLibVersion, Long],
            resultType: TYPE,
            args: (String, TYPE)*)(ev: EXPR): UserFunction[F] =
    new UserFunction(
      name = name,
      internalName = internalName,
      costByLibVersion = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ev = ev,
      args = args.map(_._1).toArray
    )

  def deprecated[F[_]](
    name: String,
    internalName: String,
    costByLibVersion: Map[StdLibVersion, Long],
    resultType: TYPE,
    args: (String, TYPE)*
  )(ev: EXPR): UserFunction[F] =
    new UserFunction[F](
      name = name,
      internalName = internalName,
      costByLibVersion = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ev = ev,
      args = args.map(_._1).toArray
    ) { override def deprecated = true }
}
