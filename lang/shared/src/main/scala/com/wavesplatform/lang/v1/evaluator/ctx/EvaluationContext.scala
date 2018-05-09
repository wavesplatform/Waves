package com.wavesplatform.lang.v1.evaluator.ctx

import cats._
import com.wavesplatform.lang.v1.FunctionHeader

case class EvaluationContext(typeDefs: Map[String, PredefType], letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, PredefFunction])

object EvaluationContext {
  val empty = EvaluationContext(Map.empty, Map.empty, Map.empty)

  implicit val monoid: Monoid[EvaluationContext] = new Monoid[EvaluationContext] {
    override val empty: EvaluationContext = EvaluationContext.empty

    override def combine(x: EvaluationContext, y: EvaluationContext): EvaluationContext =
      EvaluationContext(typeDefs = x.typeDefs ++ y.typeDefs, letDefs = x.letDefs ++ y.letDefs, functions = x.functions ++ y.functions)
  }

  def build(types: Seq[PredefType], letDefs: Map[String, LazyVal], functions: Seq[PredefFunction]): EvaluationContext =
    EvaluationContext(types.map(t => t.name -> t).toMap, letDefs, functions.map(f => f.header -> f).toMap)

  def functionCosts(xs: Iterable[PredefFunction]): Map[FunctionHeader, Long] =
    xs.map { x =>
      x.header -> x.cost
    }(collection.breakOut)
}
