package com.wavesplatform.lang.v1.evaluation

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.ctx.{Context, LazyVal, PredefFunction, PredefType}
import monix.eval.Coeval
import shapeless.{Lens, lens}

final case class EvaluationEnv(context: Context, log: StringBuffer) {
  def logAppend(l: String): Coeval[Unit] = Coeval.delay(log.append(l))
  def getLog: Coeval[String] = Coeval.delay(log.toString)
}

object EvaluationEnv {
  val context: Lens[EvaluationEnv, Context]                           = lens[EvaluationEnv] >> 'context
  val types: Lens[EvaluationEnv, Map[String, PredefType]]             = lens[EvaluationEnv] >> 'context >> 'typeDefs
  val lets: Lens[EvaluationEnv, Map[String, LazyVal]]                 = lens[EvaluationEnv] >> 'context >> 'letDefs
  val funcs: Lens[EvaluationEnv, Map[FunctionHeader, PredefFunction]] = lens[EvaluationEnv] >> 'context >> 'functions
}
