package com.wavesplatform.lang.v1.evaluator.ctx

import cats.Monad
import cats.syntax.applicative.*
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.miniev.State
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.{ContextfulNativeFunction, ContextfulUserFunction}
import com.wavesplatform.lang.v1.traits.Environment

import scala.annotation.meta.field
import scala.scalajs.js.annotation.*

sealed trait BaseFunction {
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
  implicit def header(bf: BaseFunction): FunctionHeader = bf.header
}

@JSExportTopLevel("FunctionTypeSignature")
case class FunctionTypeSignature(result: TYPE, args: Seq[(String, TYPE)], header: FunctionHeader)

@JSExportTopLevel("NativeFunction")
case class NativeFunction(
    @(JSExport @field) name: String,
    costByLibVersionMap: Map[StdLibVersion, Long],
    @(JSExport @field) signature: FunctionTypeSignature,
    ev: ContextfulNativeFunction,
    @(JSExport @field) args: Seq[String]
) extends BaseFunction

object NativeFunction {
  def withEnvironment(name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      ev: ContextfulNativeFunction
  ): NativeFunction =
    new NativeFunction(
      name = name,
      costByLibVersionMap = DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev,
      args = args.map(_._1)
    )

  def withEnvironment(name: String, costByLibVersion: Map[StdLibVersion, Long], internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      ev: ContextfulNativeFunction
  ): NativeFunction =
    new NativeFunction(
      name = name,
      costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.Native(internalName)),
      ev = ev,
      args = args.map(_._1)
    )

  def apply(name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      evl: List[EVALUATED] => Either[ExecutionError, EVALUATED]
  ): NativeFunction =
    withEnvironment(name, cost, internalName, resultType, args*)(new ContextfulNativeFunction.Simple(name, resultType, args.toSeq) {
      override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
        evl(args).pure[F]
    })

  def apply[C[_[_]]](name: String, costByLibVersion: Map[StdLibVersion, Long], internalName: Short, resultType: TYPE, args: (String, TYPE)*)(
      evl: List[EVALUATED] => Either[ExecutionError, EVALUATED]
  ): NativeFunction =
    withEnvironment(name, costByLibVersion, internalName, resultType, args*)(new ContextfulNativeFunction.Simple(name, resultType, args.toSeq) {
      override def evaluate[F[_]: Monad](env: Environment[F], args: List[EVALUATED]): F[Either[ExecutionError, EVALUATED]] =
        evl(args).pure[F]
    })
}

@JSExportTopLevel("UserFunction")
case class UserFunction(
    @(JSExport @field) name: String,
    @(JSExport @field) internalName: String,
    costByLibVersionMap: Map[StdLibVersion, Long],
    @(JSExport @field) signature: FunctionTypeSignature,
    ev: ContextfulUserFunction,
    @(JSExport @field) args: Seq[String]
) extends BaseFunction

object UserFunction {
  def withEnvironment(name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: ContextfulUserFunction): UserFunction =
    UserFunction.withEnvironment(
      name = name,
      internalName = name,
      DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap,
      resultType,
      args*
    )(ev)

  def apply(name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction =
    UserFunction.withEnvironment(name, cost, resultType, args*)(new ContextfulUserFunction {
      override def apply[F[_]: Monad](context: Environment[F], startArgs: List[EXPR]): EXPR = ev
    })

  def deprecated(name: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction =
    UserFunction.deprecated(name, name, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args*)(ev)

  def apply(name: String, costByLibVersion: Map[StdLibVersion, Long], resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction =
    UserFunction(name, name, costByLibVersion, resultType, args*)(ev)

  def apply(name: String, internalName: String, cost: Long, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction =
    UserFunction.withEnvironment(name, internalName, DirectiveDictionary[StdLibVersion].all.map(_ -> cost).toMap, resultType, args*)(
      ContextfulUserFunction.pure(ev)
    )

  def withEnvironment(
      name: String,
      internalName: String,
      costByLibVersion: Map[StdLibVersion, Long],
      resultType: TYPE,
      args: (String, TYPE)*
  )(ev: ContextfulUserFunction): UserFunction =
    new UserFunction(
      name = name,
      internalName = internalName,
      costByLibVersionMap = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ev = ev,
      args = args.map(_._1)
    )

  def apply(
      name: String,
      internalName: String,
      costByLibVersion: Map[StdLibVersion, Long],
      resultType: TYPE,
      args: (String, TYPE)*
  )(ev: EXPR): UserFunction =
    withEnvironment(name, internalName, costByLibVersion, resultType, args*)(
      ContextfulUserFunction.pure(ev)
    )

  def deprecated(
      name: String,
      internalName: String,
      costByLibVersion: Map[StdLibVersion, Long],
      resultType: TYPE,
      args: (String, TYPE)*
  )(ev: EXPR): UserFunction =
    new UserFunction(
      name = name,
      internalName = internalName,
      costByLibVersionMap = costByLibVersion,
      signature = FunctionTypeSignature(result = resultType, args = args.map(a => (a._1, a._2)), header = FunctionHeader.User(internalName, name)),
      ContextfulUserFunction.pure(ev),
      args = args.map(_._1)
    ) {
      override def deprecated = true
    }
}

abstract class ExtendedInternalFunction(delegate: BaseFunction) extends BaseFunction {

  override def signature: FunctionTypeSignature              = delegate.signature
  override def name: String                                  = delegate.name
  override def args: Seq[String]                             = delegate.args
  override val costByLibVersionMap: Map[StdLibVersion, Long] = delegate.costByLibVersionMap

  def buildExpression(state: State, args: List[EVALUATED]): Either[ExecutionError, EXPR]
}
