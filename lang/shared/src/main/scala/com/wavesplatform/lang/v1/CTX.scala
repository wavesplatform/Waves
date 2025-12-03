package com.wavesplatform.lang.v1

import cats.{Id, Monad, Monoid}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.CompilerContext.{FunctionInfo, VariableInfo}
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, LazyVal}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.{Contextful, ContextfulVal}
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos

import scala.annotation.meta.field
import scala.scalajs.js.annotation.*

@JSExportTopLevel("CTX")
case class CTX[C[_[_]]](
    @(JSExport @field) types: Seq[FINAL],
    @(JSExport @field) vars: Map[String, (FINAL, ContextfulVal[C])],
    @(JSExport @field) functions: Array[BaseFunction[C]]
) {
  lazy val typeDefs    = types.view.map(t => t.name -> t).toMap
  lazy val functionMap = functions.view.map(f => f.header -> f).toMap

  def evaluationContext[F[_]: Monad](env: C[F]): EvaluationContext[C, F] = {

    if (functionMap.size != functions.length) {
      val dups = functions.groupBy(_.header).filter(_._2.length != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(
      env,
      typeDefs,
      vars.map { case (k, v) => k -> LazyVal.fromEval(v._2(env)) },
      functionMap
    )
  }

  def evaluationContext[F[_]: Monad](implicit ev: NoContext[F] =:= C[F]): EvaluationContext[C, F] =
    evaluationContext[F](Contextful.empty[F])

  def withEnvironment[D[_[_]]](implicit ev: C[Id] =:= NoContext[Id]): CTX[D] =
    asInstanceOf[CTX[D]]

  lazy val compilerContext: CompilerContext = CompilerContext(
    typeDefs,
    vars.view.mapValues(v => VariableInfo(AnyPos, v._1)).toMap,
    functions.groupBy(_.name).map { case (k, v) => k -> FunctionInfo(AnyPos, v.map(_.signature).toList) },
    provideRuntimeTypeOnCastError = functions.exists(_ == PureContext._getType)
  )

  val opsNames = BinaryOperation.opsByPriority
    .flatMap({
      case Left(l)  => l
      case Right(l) => l
    })
    .map(x => BinaryOperation.opsToFunctions(x))
    .toSet

  lazy val decompilerContext: DecompilerContext = DecompilerContext(
    opCodes = compilerContext.functionDefs.view
      .mapValues(_.fSigList.map(_.header).filter(_.isInstanceOf[Native]).map(_.asInstanceOf[Native].name))
      .toList
      .flatMap { case (name, codes) => codes.map((_, name)) }
      .toMap,
    binaryOps = compilerContext.functionDefs.view
      .filterKeys(opsNames(_))
      .mapValues(
        _.fSigList
          .map(_.header)
          .filter(_.isInstanceOf[Native])
          .map(_.asInstanceOf[Native].name)
      )
      .toList
      .flatMap { case (name, codes) => codes.map((_, name)) }
      .toMap,
    ident = 0
  )
}

object CTX {
  val empty: CTX[NoContext] = CTX[NoContext](Seq.empty, Map.empty, Array.empty)

  implicit def monoid[C[_[_]]]: Monoid[CTX[C]] = new Monoid[CTX[C]] {
    override val empty: CTX[C] = CTX.empty.withEnvironment[C]

    override def combine(x: CTX[C], y: CTX[C]): CTX[C] =
      CTX[C](x.types ++ y.types, x.vars ++ y.vars, x.functions ++ y.functions)
  }
}
