package com.wavesplatform.lang.v1.evaluator.ctx

import cats._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.LET
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.{Contextful, LetExecResult, LetLogCallback}
import shapeless.{Lens, lens}

import java.util

case class EvaluationContext[C[_[_]], F[_]](
   environment: C[F],
   typeDefs : Map[String, FINAL],
   letDefs  : Map[String, LazyVal[F]],
   functions: Map[FunctionHeader, BaseFunction[C]]
) {
  def mapK[G[_] : Monad](f: F ~> G): EvaluationContext[C, G] =
    EvaluationContext(
      environment.asInstanceOf[C[G]],
      typeDefs,
      letDefs.view.mapValues(_.mapK(f)).toMap,
      functions
    )
}

case class LoggedEvaluationContext[C[_[_]], F[_]](l: LetLogCallback[F], ec: EvaluationContext[C, F]) {
  val loggedLets: util.IdentityHashMap[LET, Unit] = new util.IdentityHashMap()

  def log(let: LET, result: LetExecResult[F]): Unit =
    loggedLets.computeIfAbsent(let, _ => l(let.name)(result))
}

object LoggedEvaluationContext {
  class Lenses[F[_], C[_[_]]] {
    val types: Lens[LoggedEvaluationContext[C, F], Map[String, FINAL]]                   = lens[LoggedEvaluationContext[C, F]] >> Symbol("ec") >> Symbol("typeDefs")
    val lets: Lens[LoggedEvaluationContext[C, F], Map[String, LazyVal[F]]]               = lens[LoggedEvaluationContext[C, F]] >> Symbol("ec") >> Symbol("letDefs")
    val funcs: Lens[LoggedEvaluationContext[C, F], Map[FunctionHeader, BaseFunction[C]]] = lens[LoggedEvaluationContext[C, F]] >> Symbol("ec") >> Symbol("functions")
  }
}

object EvaluationContext {

  val empty = EvaluationContext(Contextful.empty[Id], Map.empty, Map.empty, Map.empty)

  implicit def monoid[F[_], C[_[_]]]: Monoid[EvaluationContext[C, F]] = new Monoid[EvaluationContext[C, F]] {
    override val empty: EvaluationContext[C, F] = EvaluationContext.empty.asInstanceOf[EvaluationContext[C, F]]

    override def combine(x: EvaluationContext[C, F], y: EvaluationContext[C, F]): EvaluationContext[C, F] =
      EvaluationContext(
        environment = y.environment,
        typeDefs = x.typeDefs ++ y.typeDefs,
        letDefs = x.letDefs ++ y.letDefs,
        functions = x.functions ++ y.functions
      )
  }

  def build[F[_], C[_[_]]](
    environment: C[F],
    typeDefs:    Map[String, FINAL],
    letDefs:     Map[String, LazyVal[F]],
    functions:   Seq[BaseFunction[C]]
  ): EvaluationContext[C, F] = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(environment, typeDefs, letDefs, functions.map(f => f.header -> f).toMap)
  }

  def build(
    typeDefs:    Map[String, FINAL],
    letDefs:     Map[String, LazyVal[Id]],
    functions:   Seq[BaseFunction[NoContext]] = Seq()
  ): EvaluationContext[NoContext, Id] = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext[NoContext, Id](Contextful.empty[Id], typeDefs, letDefs, functions.map(f => f.header -> f).toMap)
  }
}
