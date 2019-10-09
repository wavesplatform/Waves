package com.wavesplatform.lang.v1

import cats.{Id, Monad, Monoid, Semigroup, ~>}
import cats.implicits._
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation

import scala.annotation.meta.field
import scala.scalajs.js.annotation._

@JSExportTopLevel("CTX")
case class CTX[F[_], C](
  environment: C,
  @(JSExport @field) types: Seq[FINAL],
  @(JSExport @field) vars: Map[String, (FINAL, LazyVal[F])],
  @(JSExport @field) functions: Array[BaseFunction[F, C]]
) {
  lazy val typeDefs = types.map(t => t.name -> t).toMap
  lazy val evaluationContext: EvaluationContext[F, C] = {
    if (functions.map(_.header).distinct.length != functions.length) {
      val dups = functions.groupBy(_.header).filter(_._2.length != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(environment, typeDefs, vars.mapValues(_._2), functions.map(f => f.header -> f).toMap)
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

  def mapK[G[_] : Monad](f: F ~> G): CTX[G, C] =
    copy(
      functions = functions.map(_.mapK(f)),
      vars = vars.mapValues { case(t, value) => (t, value.mapK(f)) }
    )
}
object CTX {
  val empty: CTX[Id, _] = CTX(???, Seq.empty, Map.empty, Array.empty[BaseFunction[Id, _]])

  implicit def monoid[F[_], C]: Monoid[CTX[F, C]] = new Monoid[CTX[F, C]] {
    override val empty: CTX[F, C] =
      CTX.empty.asInstanceOf[CTX[F, C]]

    override def combine(x: CTX[F, C], y: CTX[F, C]): CTX[F, C] =
      CTX(
        x.environment,
        x.types ++ y.types,
        x.vars ++ y.vars,
        x.functions ++ y.functions
      )
  }
}
