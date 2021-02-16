package com.wavesplatform.lang.v1.evaluator.ctx

import cats.Id
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.traits.Environment

case class InvariableContext(private val ctx: CTX[Environment]) {
  private val constants                  = ctx.vars.collect { case (k, v) if v._2.isPure  => k -> LazyVal.fromEval(v._2(null)) }
  private def vars(env: Environment[Id]) = ctx.vars.collect { case (k, v) if !v._2.isPure => k -> LazyVal.fromEval(v._2(env)) }

  private val rawEvaluationContext: EvaluationContext[Environment, Id] =
    EvaluationContext[Environment, Id](
      null,
      ctx.typeDefs,
      constants,
      ctx.functionMap
    )

  def completeContext(env: Environment[Id]): EvaluationContext[Environment, Id] =
    rawEvaluationContext.copy(environment = env, letDefs = constants ++ vars(env))
}
