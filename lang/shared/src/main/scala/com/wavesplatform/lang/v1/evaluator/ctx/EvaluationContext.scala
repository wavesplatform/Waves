package com.wavesplatform.lang.v1.evaluator.ctx

import cats._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.LetLogCallback
import shapeless.{Lens, lens}

case class EvaluationContext(typeDefs: Map[String, DefinedType], letDefs: Map[String, LazyVal], functions: Map[FunctionHeader, BaseFunction])

case class LoggedEvaluationContext(l: LetLogCallback, ec: EvaluationContext)

object LoggedEvaluationContext {
  object Lenses {
    val types: Lens[LoggedEvaluationContext, Map[String, DefinedType]]          = lens[LoggedEvaluationContext] >> 'ec >> 'typeDefs
    val lets: Lens[LoggedEvaluationContext, Map[String, LazyVal]]               = lens[LoggedEvaluationContext] >> 'ec >> 'letDefs
    val funcs: Lens[LoggedEvaluationContext, Map[FunctionHeader, BaseFunction]] = lens[LoggedEvaluationContext] >> 'ec >> 'functions
  }
}

object EvaluationContext {

  val empty = EvaluationContext(Map.empty, Map.empty, Map.empty)

  implicit val monoid: Monoid[EvaluationContext] = new Monoid[EvaluationContext] {
    override val empty: EvaluationContext = EvaluationContext.empty

    override def combine(x: EvaluationContext, y: EvaluationContext): EvaluationContext =
      EvaluationContext(typeDefs = x.typeDefs ++ y.typeDefs, letDefs = x.letDefs ++ y.letDefs, functions = x.functions ++ y.functions)
  }

  def build(typeDefs: Map[String, DefinedType], letDefs: Map[String, LazyVal], functions: Seq[BaseFunction]): EvaluationContext = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(typeDefs = typeDefs, letDefs = letDefs, functions = functions.map(f => f.header -> f).toMap)
  }
}
