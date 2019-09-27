package com.wavesplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import cats.implicits._
import cats.{Eval, Monad, ~>}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types._

import scala.annotation.meta.field
import scala.scalajs.js.annotation._

sealed trait BaseFunction[F[_]] {
  @JSExport def signature: FunctionTypeSignature
  @JSExport def header: FunctionHeader = signature.header
  def costByLibVersion: Map[StdLibVersion, Long]
  @JSExport def name: String
  @JSExport def args: Array[String]
  @JSExport def deprecated: Boolean = false

  def mapK[G[_]](f: F ~> G): BaseFunction[G]
}

object BaseFunction {
  implicit def header[F[_]](bf: BaseFunction[F]): FunctionHeader = bf.header
}

@JSExportTopLevel("FunctionTypeSignature")
case class FunctionTypeSignature(result: TYPE, args: Seq[(String, TYPE)], header: FunctionHeader)

@JSExportTopLevel("NativeFunction")
case class NativeFunction[F[_]](
  @(JSExport @field) name: String,
  costByLibVersion: Map[StdLibVersion, Long],
  @(JSExport @field) signature: FunctionTypeSignature,
  ev: List[EVALUATED] => F[Either[ExecutionError, EVALUATED]],
  @(JSExport @field) args: Array[String]
) extends BaseFunction[F] {
  def eval(args: List[EVALUATED]): TrampolinedExecResult[F, EVALUATED] =
    EitherT.apply[λ[q => Eval[F[q]]], ExecutionError, EVALUATED](ev(args).pure[Eval])

  override def mapK[G[_]](f: F ~> G): BaseFunction[G] =
    copy(ev = ev.andThen(evalArgs => f(evalArgs)))
}

object NativeFunction {

  def apply[F[_] : Monad](name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      ev: PartialFunction[List[EVALUATED], F[Either[ExecutionError, EVALUATED]]]): NativeFunction[F] =
    new NativeFunction(
      name = name,
      costByLibVersion = DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev.orElse { case _ => "Passed argument with wrong type".asLeft[EVALUATED].pure[F] },
      args = args.map(_._1).toArray
    )

  def apply[F[_] : Monad](name: String,
            costByLibVersion: Map[StdLibVersion, Long],
            internalName: Short,
            resultType: TYPE,
            args: (String, TYPE, String)*)(ev: List[EVALUATED] => F[Either[ExecutionError, EVALUATED]]): NativeFunction[F] =
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
) extends BaseFunction[F] {

  override def mapK[G[_]](f: F ~> G): BaseFunction[G] =
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
