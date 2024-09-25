package com.wavesplatform.lang.v1.evaluator.ctx

import cats.Monad
import cats.syntax.applicative._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.{ContextfulNativeFunction, ContextfulUserFunction}

import scala.annotation.meta.field
import scala.scalajs.js.annotation._

sealed trait BaseFunction[C[_[_]]] {
  @JSExport def signature: FunctionTypeSignature
  @JSExport def header: FunctionHeader = signature.header
  @JSExport def name: String
  @JSExport def args: Seq[String]
  @JSExport def deprecated: Boolean = false

  val costByLibVersionMap: Map[StdLibVersion, Long]

  def costByLibVersion(version: StdLibVersion): Long =
    costByLibVersionMap.getOrElse(version, costByLibVersionMap.maxBy(_._1)._2)
}

object BaseFunction {
  implicit def header[C[_[_]]](bf: BaseFunction[C]): FunctionHeader = bf.header
}

@JSExportTopLevel("FunctionTypeSignature")
case class FunctionTypeSignature(result: TYPE, args: Seq[(String, TYPE)], header: FunctionHeader)

@JSExportTopLevel("NativeFunction")
case class NativeFunction[C[_[_]]](
    @(JSExport @field) name: String,
    costByLibVersionMap: Map[StdLibVersion, Long],
    @(JSExport @field) signature: FunctionTypeSignature,
    ev: ContextfulNativeFunction[C],
    @(JSExport @field) args: Seq[String]
) extends BaseFunction[C]

object NativeFunction {
  def withEnvironment[C[_[_]]](name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      ev: ContextfulNativeFunction[C]
  ): NativeFunction[C] =
    new NativeFunction(
      name = name,
      costByLibVersionMap = DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev /*ev.orElse { case _ => "Passed argument with wrong type".asLeft[EVALUATED].pure[F] }(_, _)*/,
      args = args.map(_._1)
    )

  def withEnvironment[C[_[_]]](
      name: String,
      costByLibVersion: Map[StdLibVersion, Long],
      internalName: Short,
      resultType: TYPE,
      args: (String, TYPE)*
  )(ev: ContextfulNativeFunction[C]): NativeFunction[C] =
    new NativeFunction(
      name = name,
      costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev,
      args = args.map(_._1)
    )

  def apply[C[_[_]]](name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      evl: List[EVALUATED] => Either[ExecutionError, EVALUATED]
  ): NativeFunction[C] =
    withEnvironment[C](name, cost, internalName, resultType, args*)(new ContextfulNativeFunction.Simple[C](name, resultType, args.toSeq) {
      override def evaluate[F[_]: Monad](env: C[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
        evl(args).pure[F]
    })

  def apply[C[_[_]]](name: String, costByLibVersion: Map[StdLibVersion, Long], internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      evl: List[EVALUATED] => Either[ExecutionError, EVALUATED]
  ): NativeFunction[C] =
    withEnvironment[C](name, costByLibVersion, internalName, resultType, args*)(new ContextfulNativeFunction.Simple[C](name, resultType, args.toSeq) {
      override def evaluate[F[_]: Monad](env: C[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
        evl(args).pure[F]
    })
}

@JSExportTopLevel("UserFunction")
case class UserFunction[C[_[_]]](
    @(JSExport @field) name: String,
    @(JSExport @field) internalName: String,
    costByLibVersionMap: Map[StdLibVersion, Long],
    @(JSExport @field) signature: FunctionTypeSignature,
    ev: ContextfulUserFunction[C],
    @(JSExport @field) args: Seq[String]
) extends BaseFunction[C]

object UserFunction {
  def withEnvironment[C[_[_]]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: ContextfulUserFunction[C]): UserFunction[C] =
    UserFunction.withEnvironment(
      name = name,
      internalName = name,
      DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      resultType,
      args*
    )(ev)

  def apply[C[_[_]]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[C] =
    UserFunction.withEnvironment[C](name, cost, resultType, args*)(new ContextfulUserFunction[C] {
      override def apply[F[_]: Monad](context: C[F], startArgs: List[EXPR]): EXPR = ev
    })

  def deprecated[C[_[_]]](name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[C] =
    UserFunction.deprecated(name, name, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args*)(ev)

  def apply[C[_[_]]](name: String, costByLibVersion: Map[StdLibVersion, Long], resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[C] =
    UserFunction(name, name, costByLibVersion, resultType, args*)(ev)

  def apply[C[_[_]]](name: String, internalName: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction[C] =
    UserFunction.withEnvironment[C](name, internalName, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args*)(
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
      costByLibVersionMap = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ev = ev,
      args = args.map(_._1)
    )

  def apply[C[_[_]]](
      name: String,
      internalName: String,
      costByLibVersion: Map[StdLibVersion, Long],
      resultType: TYPE,
      args: (String, TYPE)*
  )(ev: EXPR): UserFunction[C] =
    withEnvironment[C](name, internalName, costByLibVersion, resultType, args*)(
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
      costByLibVersionMap = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ContextfulUserFunction.pure[C](ev),
      args = args.map(_._1)
    ) {
      override def deprecated = true
    }
}
