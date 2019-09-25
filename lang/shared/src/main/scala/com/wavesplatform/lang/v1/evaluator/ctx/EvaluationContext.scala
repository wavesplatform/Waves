package com.wavesplatform.lang.v1.evaluator.ctx

import cats._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.LetLogCallback
import shapeless.{Lens, lens}

case class EvaluationContext[F[_]](
   typeDefs : Map[String, FINAL],
   letDefs  : Map[String, LazyVal[F]],
   functions: Map[FunctionHeader, BaseFunction[F]]
)

case class LoggedEvaluationContext[F[_]](l: LetLogCallback[F], ec: EvaluationContext[F])

object LoggedEvaluationContext {
  class Lenses[F[_]] {
    val types: Lens[LoggedEvaluationContext[F], Map[String, FINAL]]                   = lens[LoggedEvaluationContext[F]] >> 'ec >> 'typeDefs
    val lets: Lens[LoggedEvaluationContext[F], Map[String, LazyVal[F]]]               = lens[LoggedEvaluationContext[F]] >> 'ec >> 'letDefs
    val funcs: Lens[LoggedEvaluationContext[F], Map[FunctionHeader, BaseFunction[F]]] = lens[LoggedEvaluationContext[F]] >> 'ec >> 'functions
  }
}

object EvaluationContext {

  val empty = EvaluationContext(Map.empty, Map.empty, Map.empty)

  implicit def monoid[F[_]]: Monoid[EvaluationContext[F]] = new Monoid[EvaluationContext[F]] {
    override val empty: EvaluationContext[F] = EvaluationContext.empty.asInstanceOf[EvaluationContext[F]]

    override def combine(x: EvaluationContext[F], y: EvaluationContext[F]): EvaluationContext[F] =
      EvaluationContext(typeDefs = x.typeDefs ++ y.typeDefs, letDefs = x.letDefs ++ y.letDefs, functions = x.functions ++ y.functions)
  }

  def build[F[_]](typeDefs: Map[String, FINAL], letDefs: Map[String, LazyVal[F]], functions: Seq[BaseFunction[F]]): EvaluationContext[F] = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(typeDefs = typeDefs, letDefs = letDefs, functions = functions.map(f => f.header -> f).toMap)
  }
}
