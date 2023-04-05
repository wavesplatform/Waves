package com.wavesplatform.lang.v1.evaluator.ctx

import cats.*
import cats.syntax.functor.*
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.LET
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.{Contextful, LetExecResult, LetLogCallback}
import shapeless.{Lens, lens}

import java.util

case class EvaluationContext[C[_[_]], F[_]](
    environment: C[F],
    typeDefs: Map[String, FINAL],
    letDefs: Map[String, LazyVal[F]],
    functions: Map[FunctionHeader, BaseFunction[C]]
) {
  def mapK[G[_]: Monad](f: F ~> G): EvaluationContext[C, G] =
    EvaluationContext(
      environment.asInstanceOf[C[G]],
      typeDefs,
      letDefs.view.mapValues(_.mapK(f)).toMap,
      functions
    )
}

trait LoggedEvaluationContext[C[_[_]], F[_]] {
  def ec: EvaluationContext[C, F]
  def log(let: LET, result: LetExecResult[F]): Unit
}

case class EnabledLogEvaluationContext[C[_[_]], F[_]: Monad](l: LetLogCallback[F], ec: EvaluationContext[C, F])
    extends LoggedEvaluationContext[C, F] {
  val loggedLets: util.IdentityHashMap[LET, Unit]          = new util.IdentityHashMap()
  val loggedErrors: collection.mutable.Set[ExecutionError] = collection.mutable.Set()

  override def log(let: LET, result: LetExecResult[F]): Unit = {
    result.map {
      case Left(err) if !loggedErrors.contains(err) =>
        loggedErrors.addOne(err)
        add(let, result)
      case Left(_) => ()
      case _       => add(let, result)
    }
  }

  private def add(let: LET, result: LetExecResult[F]): Unit =
    loggedLets.computeIfAbsent(let, _ => l(let.name)(result))
}

object EnabledLogEvaluationContext {
  class Lenses[F[_]: Monad, C[_[_]]] {
    val types: Lens[EnabledLogEvaluationContext[C, F], Map[String, FINAL]] =
      lens[EnabledLogEvaluationContext[C, F]] >> Symbol("ec") >> Symbol("typeDefs")
    val lets: Lens[EnabledLogEvaluationContext[C, F], Map[String, LazyVal[F]]] =
      lens[EnabledLogEvaluationContext[C, F]] >> Symbol("ec") >> Symbol("letDefs")
    val funcs: Lens[EnabledLogEvaluationContext[C, F], Map[FunctionHeader, BaseFunction[C]]] =
      lens[EnabledLogEvaluationContext[C, F]] >> Symbol("ec") >> Symbol("functions")
  }
}

case class DisabledLogEvaluationContext[C[_[_]], F[_]](ec: EvaluationContext[C, F]) extends LoggedEvaluationContext[C, F] {
  override def log(let: LET, result: LetExecResult[F]): Unit = ()
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
      typeDefs: Map[String, FINAL],
      letDefs: Map[String, LazyVal[F]],
      functions: Seq[BaseFunction[C]]
  ): EvaluationContext[C, F] = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(environment, typeDefs, letDefs, functions.map(f => f.header -> f).toMap)
  }

  def build(
      typeDefs: Map[String, FINAL],
      letDefs: Map[String, LazyVal[Id]],
      functions: Seq[BaseFunction[NoContext]] = Seq()
  ): EvaluationContext[NoContext, Id] = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext[NoContext, Id](Contextful.empty[Id], typeDefs, letDefs, functions.map(f => f.header -> f).toMap)
  }
}
