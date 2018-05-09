package com.wavesplatform.lang.v1.evaluator.ctx

import com.wavesplatform.lang.v1.FunctionHeader
import shapeless._

final case class LoggedEvaluationContext(context: EvaluationContext, log: List[String] = List.empty) {
  def logAppend(l: String): LoggedEvaluationContext = copy(log = l :: log)
  def getLog: String                                = log.map(_.trim).reverse mkString ("", "\n", "")
}

object LoggedEvaluationContext {
  val context: Lens[LoggedEvaluationContext, EvaluationContext]                 = lens[LoggedEvaluationContext] >> 'context
  val types: Lens[LoggedEvaluationContext, Map[String, PredefType]]             = lens[LoggedEvaluationContext] >> 'context >> 'typeDefs
  val lets: Lens[LoggedEvaluationContext, Map[String, LazyVal]]                 = lens[LoggedEvaluationContext] >> 'context >> 'letDefs
  val funcs: Lens[LoggedEvaluationContext, Map[FunctionHeader, PredefFunction]] = lens[LoggedEvaluationContext] >> 'context >> 'functions
}
