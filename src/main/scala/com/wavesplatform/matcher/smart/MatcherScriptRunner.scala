package com.wavesplatform.matcher.smart

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.ExprEvaluator.Log
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.script.Script
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply[A](script: Script, order: Order): (Log, Either[String, A]) = script match {
    case Script.Expr(expr) =>
      val ctx = MatcherContext.build(AddressScheme.current.chainId, Coeval.evalOnce(order))
      EvaluatorV1.applywithLogging[A](ctx, expr)
    case _ => (List.empty, "Unsupported script version".asLeft[A])
  }
}
