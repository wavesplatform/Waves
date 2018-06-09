package com.wavesplatform.lang.v1.evaluator.ctx

import cats._
import com.wavesplatform.lang.v1.FunctionHeader
import shapeless.{Lens, lens}

case class EvaluationContext(letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, BaseFunction])

object EvaluationContext {

  object Lenses {
    val lets: Lens[EvaluationContext, Map[String, LazyVal]]               = lens[EvaluationContext] >> 'letDefs
    val funcs: Lens[EvaluationContext, Map[FunctionHeader, BaseFunction]] = lens[EvaluationContext] >> 'functions
  }

  val empty = EvaluationContext(Map.empty, Map.empty)

  implicit val monoid: Monoid[EvaluationContext] = new Monoid[EvaluationContext] {
    override val empty: EvaluationContext = EvaluationContext.empty

    override def combine(x: EvaluationContext, y: EvaluationContext): EvaluationContext =
      EvaluationContext(letDefs = x.letDefs ++ y.letDefs, functions = x.functions ++ y.functions)
  }

  def build(letDefs: Map[String, LazyVal], functions: Seq[BaseFunction]): EvaluationContext = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(letDefs = letDefs, functions = functions.map(f => f.header -> f).toMap)
  }

  def functionCosts(xs: Iterable[BaseFunction]): Map[FunctionHeader, Long] =
    xs.map { x =>
      x.header -> x.cost
    }(collection.breakOut)
}
