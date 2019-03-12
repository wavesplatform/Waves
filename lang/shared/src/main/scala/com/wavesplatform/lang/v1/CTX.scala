package com.wavesplatform.lang.v1

import cats.Monoid
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.ctx._

import scala.annotation.meta.field
import scala.scalajs.js.annotation._

import com.wavesplatform.lang.v1.parser.BinaryOperation

@JSExportTopLevel("CTX")
case class CTX(@(JSExport @field) types: Seq[DefinedType],
               @(JSExport @field) vars: Map[String, ((FINAL, String), LazyVal)],
               @(JSExport @field) functions: Array[BaseFunction]) {
  lazy val typeDefs = types.map(t => t.name -> t).toMap
  lazy val evaluationContext: EvaluationContext = {
    if (functions.map(_.header).distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(typeDefs = typeDefs, letDefs = vars.mapValues(_._2), functions = functions.map(f => f.header -> f).toMap)
  }
  lazy val compilerContext: CompilerContext = CompilerContext(
    predefTypes = typeDefs,
    varDefs = vars.mapValues(_._1),
    functionDefs = functions.groupBy(_.name).map { case (k, v) => k -> v.map(_.signature).toList }
  )

  val opsNames = BinaryOperation.opsByPriority
    .flatMap({
      case Left(l)  => l
      case Right(l) => l
    })
    .map(x => BinaryOperation.opsToFunctions(x))
    .toSet

  lazy val decompilerContext: DecompilerContext = DecompilerContext(
    opCodes = compilerContext.functionDefs
      .mapValues(_.map(_.header).filter(_.isInstanceOf[Native]).map(_.asInstanceOf[Native].name))
      .toList
      .flatMap { case (name, codes) => codes.map((_, name)) }
      .toMap,
    binaryOps = compilerContext.functionDefs
      .filterKeys(opsNames(_))
      .mapValues(
        _.map(_.header)
          .filter(_.isInstanceOf[Native])
          .map(_.asInstanceOf[Native].name))
      .toList
      .flatMap { case (name, codes) => codes.map((_, name)) }
      .toMap,
    ident = 0
  )

}
object CTX {
  val empty = CTX(Seq.empty, Map.empty, Array.empty)

  implicit val monoid: Monoid[CTX] = new Monoid[CTX] {
    override val empty: CTX                   = CTX.empty
    override def combine(x: CTX, y: CTX): CTX = CTX(x.types ++ y.types, x.vars ++ y.vars, x.functions ++ y.functions)
  }
}
