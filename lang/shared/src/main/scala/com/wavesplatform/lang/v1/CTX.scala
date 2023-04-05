package com.wavesplatform.lang.v1

import cats.{Monad, Monoid}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.CompilerContext.{FunctionInfo, VariableInfo}
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{BaseFunction, EvaluationContext, LazyVal}
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.traits.Environment

import scala.annotation.meta.field
import scala.scalajs.js.annotation.*

@JSExportTopLevel("CTX")
case class CTX(
    @(JSExport @field) types: Seq[FINAL],
    @(JSExport @field) vars: Map[String, (FINAL, ContextfulVal)],
    @(JSExport @field) functions: Array[BaseFunction]
) {
  lazy val typeDefs    = types.view.map(t => t.name -> t).toMap
  lazy val functionMap = functions.view.map(f => f.header -> f).toMap

  def evaluationContext[F[_]: Monad](env: Environment[F]): EvaluationContext[F] = {

//    if (functionMap.size != functions.length) {
//      val dups = functions.groupBy(_.header).filter(_._2.length != 1)
//      throw new Exception(s"Duplicate runtime functions names: $dups")
//    }
    EvaluationContext(
      env,
      typeDefs,
      vars.map { case (k, v) => k -> LazyVal.fromEval(v._2(env)) },
      functionMap
    )
  }

  lazy val compilerContext: CompilerContext = CompilerContext(
    typeDefs,
    vars.view.mapValues(v => VariableInfo(AnyPos, v._1)).toMap,
    functions.groupBy(_.name).map { case (k, v) => k -> FunctionInfo(AnyPos, v.map(_.signature).toList) },
    provideRuntimeTypeOnCastError = functions.contains(PureContext._getType)
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
  val empty: CTX = CTX(Seq.empty, Map.empty, Array.empty)

  implicit def monoid: Monoid[CTX] = new Monoid[CTX] {
    override val empty: CTX = CTX.empty

    override def combine(x: CTX, y: CTX): CTX =
      CTX(x.types ++ y.types, x.vars ++ y.vars, x.functions ++ y.functions)
  }
}
