package com.wavesplatform.lang.v1

import cats.{Id, Monoid}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation

import scala.annotation.meta.field
import scala.scalajs.js.annotation._

@JSExportTopLevel("CTX")
case class CTX[F[_]](
  @(JSExport @field) types: Seq[FINAL],
  @(JSExport @field) vars: Map[String, (FINAL, LazyVal[F])],
  @(JSExport @field) functions: Array[BaseFunction[F]]
) {
  lazy val typeDefs = types.map(t => t.name -> t).toMap
  lazy val evaluationContext: EvaluationContext[F] = {
    if (functions.map(_.header).distinct.length != functions.length) {
      val dups = functions.groupBy(_.header).filter(_._2.length != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(typeDefs = typeDefs, letDefs = vars.mapValues(_._2), functions = functions.map(f => f.header -> f).toMap)
  }
  lazy val compilerContext: CompilerContext = CompilerContext(
    typeDefs,
    vars.mapValues(_._1),
    functions.groupBy(_.name).map { case (k, v) => k -> v.map(_.signature).toList }
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
  val empty: CTX[Id] = CTX(Seq.empty, Map.empty, Array.empty[BaseFunction[Id]])

  implicit def monoid[F[_]]: Monoid[CTX[F]] = new Monoid[CTX[F]] {
    override val empty: CTX[F]                   = CTX.empty.asInstanceOf[CTX[F]]
    override def combine(x: CTX[F], y: CTX[F]): CTX[F] = CTX(x.types ++ y.types, x.vars ++ y.vars, x.functions ++ y.functions)
  }
}
