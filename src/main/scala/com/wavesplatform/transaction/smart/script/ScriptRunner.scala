package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.{ExecutionError, ExprEvaluator}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.BlockchainContext
import monix.eval.Coeval
import shapeless._

object ScriptRunner {

  def apply[A <: EVALUATED](height: Int,
                            in: Transaction :+: Order :+: CNil,
                            blockchain: Blockchain,
                            script: Script,
                            isTokenScript: Boolean): (ExprEvaluator.Log, Either[ExecutionError, A]) = {
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          script.version,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        EvaluatorV1.applywithLogging[A](ctx, expr)

      case _ => (List.empty, "Unsupported script version".asLeft[A])
    }
  }
}
