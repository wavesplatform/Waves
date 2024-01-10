package com.wavesplatform.lang.v1.evaluator.ctx

import cats.Id
import com.wavesplatform.lang.utils.environment
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.traits.Environment

case class InvariableContext(private val ctx: CTX) {
  private val constants                  = ctx.vars.collect { case (k, v) if v._2.isPure => k -> LazyVal.fromEval(v._2(environment)) }
  private def vars(env: Environment[Id]) = ctx.vars.collect { case (k, v) if !v._2.isPure => k -> LazyVal.fromEval(v._2(env)) }

  private val rawEvaluationContext: EvaluationContext[Id] =
    EvaluationContext[Id](
      environment,
      ctx.typeDefs,
      constants,
      ctx.functionMap
    )

  def completeContext(env: Environment[Id]): EvaluationContext[Id] =
    rawEvaluationContext.copy(environment = env, letDefs = constants ++ vars(env))
}
