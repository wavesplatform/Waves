package com.wavesplatform.matcher.smart

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, Log}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.script.Script
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply[A <: EVALUATED](script: Script, order: Order): (Log, Either[String, A]) = script match {
    case Script.Expr(expr) =>
      val ctx = MatcherContext.build(script.version, AddressScheme.current.chainId, Coeval.evalOnce(order))
      EvaluatorV1.applywithLogging[A](ctx, expr)
    case _ => (List.empty, "Unsupported script version".asLeft[A])
  }
}
