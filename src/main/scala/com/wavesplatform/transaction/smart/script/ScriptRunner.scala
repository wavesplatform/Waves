package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.state._
import monix.eval.Coeval
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.BlockchainContext
import shapeless._

object ScriptRunner {

  def apply[A](height: Int,
               in: Transaction :+: Order :+: CNil,
               blockchain: Blockchain,
               script: Script): (EvaluationContext, Either[ExecutionError, A]) =
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain
        )
        EvaluatorV1[A](ctx, expr)

      case _ => (EvaluationContext.empty, "Unsupported script version".asLeft[A])
    }

}
