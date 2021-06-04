package com.wavesplatform.lang.v1

import cats.implicits._
import com.wavesplatform.lang.{Global, Common}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.Environment
import monix.eval.Coeval

package object estimator {
  private val version = V3
  private val ctx =
    PureContext.build(version, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
    WavesContext.build(Global, DirectiveSet.contractDirectiveSet)

  private val environment = Common.emptyBlockchainEnvironment()
  private val evaluator =
    new EvaluatorV2(LoggedEvaluationContext(_ => _ => (), ctx.evaluationContext(environment)), version)

  val evaluatorV2AsEstimator = new ScriptEstimator {
    override val version: Int = 0

    override def apply(declaredVals: Set[String], functionCosts: Map[FunctionHeader, Coeval[Long]], expr: Terms.EXPR): Either[String, Long] =
      Right(evaluator(expr, 4000)._2)
  }
}
