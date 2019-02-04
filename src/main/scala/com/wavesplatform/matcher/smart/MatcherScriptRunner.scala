package com.wavesplatform.matcher.smart

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, Log}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply(script: Script, order: Order, isTokenScript: Boolean): (Log, Either[String, EVALUATED]) = script match {
    case s: ExprScriprImpl =>
      val ctx = MatcherContext.build(script.version, AddressScheme.current.chainId, Coeval.evalOnce(order), !isTokenScript)
      EvaluatorV1.applywithLogging(ctx, s.expr)
    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
