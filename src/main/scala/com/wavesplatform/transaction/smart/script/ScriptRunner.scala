package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang._
import com.wavesplatform.lang.v1.evaluator._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.BlockchainContext
import com.wavesplatform.transaction.smart.script.v1.ScriptV1.ScriptV1Impl
import com.wavesplatform.transaction.smart.script.v1.ScriptV2
import monix.eval.Coeval
import shapeless._

object ScriptRunner {

  def apply[A <: EVALUATED](height: Int,
                            in: Transaction :+: Order :+: CNil,
                            blockchain: Blockchain,
                            script: Script,
                            proofsEnabled: Boolean): (Log, Either[ExecutionError, A]) = {
    script match {
      case s: ScriptV1Impl =>
        val ctx = BlockchainContext.build(
          script.version,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          proofsEnabled
        )
        EvaluatorV1.applywithLogging[A](ctx, s.expr)
      case s: ScriptV2 => (List.empty, Left("Transactions from contracts not supported"))
      case _           => (List.empty, "Unsupported script version".asLeft[A])
    }
  }
}
