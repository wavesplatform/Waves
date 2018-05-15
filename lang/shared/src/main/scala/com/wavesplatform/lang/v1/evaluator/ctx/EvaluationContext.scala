package com.wavesplatform.lang.v1.evaluator.ctx

import cats._
import com.wavesplatform.lang.v1.FunctionHeader

case class EvaluationContext(caseTypeDefs: Map[String, PredefCaseType], letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, PredefFunction])

object EvaluationContext {
  val empty = EvaluationContext(Map.empty, Map.empty, Map.empty)

  implicit val monoid: Monoid[EvaluationContext] = new Monoid[EvaluationContext] {
    override val empty: EvaluationContext = EvaluationContext.empty

    override def combine(x: EvaluationContext, y: EvaluationContext): EvaluationContext =
      EvaluationContext(caseTypeDefs = x.caseTypeDefs ++ y.caseTypeDefs, letDefs = x.letDefs ++ y.letDefs, functions = x.functions ++ y.functions)
  }

  def build(caseTypes: Seq[PredefCaseType], letDefs: Map[String, LazyVal], functions: Seq[PredefFunction]): EvaluationContext =
    EvaluationContext(caseTypeDefs = caseTypes.map(t => t.name -> t).toMap, letDefs = letDefs, functions = functions.map(f => f.header -> f).toMap)

  def functionCosts(xs: Iterable[PredefFunction]): Map[FunctionHeader, Long] =
    xs.map { x =>
      x.header -> x.cost
    }(collection.breakOut)
}
