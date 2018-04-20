package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.ctx.Context
import shapeless._

final case class EvaluationContext(context: Context, log: List[String] = List.empty) {
  def logAppend(l: String): EvaluationContext = copy(log = l::log)
  def getLog: String = log.map(_.trim).reverse mkString ("", "\n", "")
}

object EvaluationContext {
  val context = lens[EvaluationContext] >> 'context
  val types = lens[EvaluationContext] >> 'context >> 'typeDefs
  val lets = lens[EvaluationContext] >> 'context >> 'letDefs
  val funcs = lens[EvaluationContext] >> 'context >> 'functions
}
