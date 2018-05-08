package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.ctx.{Context, LazyVal, PredefFunction, PredefType}
import shapeless._

final case class EvaluationContext(context: Context, log: List[String] = List.empty) {
  def logAppend(l: String): EvaluationContext = copy(log = l :: log)
  def getLog: String                          = log.map(_.trim).reverse mkString ("", "\n", "")
}

object EvaluationContext {
  val context: Lens[EvaluationContext, Context]                           = lens[EvaluationContext] >> 'context
  val types: Lens[EvaluationContext, Map[String, PredefType]]             = lens[EvaluationContext] >> 'context >> 'typeDefs
  val lets: Lens[EvaluationContext, Map[String, LazyVal]]                 = lens[EvaluationContext] >> 'context >> 'letDefs
  val funcs: Lens[EvaluationContext, Map[FunctionHeader, PredefFunction]] = lens[EvaluationContext] >> 'context >> 'functions
}
