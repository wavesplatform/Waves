package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.v1.evaluator._
import com.wavesplatform.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.wavesplatform.state._
import com.wavesplatform.transaction.{Authorized, Proven, Transaction}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.{BlockchainContext, RealTransactionWrapper, Verifier}
import com.wavesplatform.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import monix.eval.Coeval
import shapeless._

object ScriptRunner {

  def apply(height: Int,
            in: Transaction :+: Order :+: CNil,
            blockchain: Blockchain,
            script: Script,
            isTokenScript: Boolean): (Log, Either[ExecutionError, EVALUATED]) = {
    script match {
      case s: ExprScriprImpl =>
        val ctx = BlockchainContext.build(
          script.version,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        EvaluatorV1.applywithLogging[EVALUATED](ctx, s.expr)
      case ContractScript.ContractScriptImpl(_, Contract(_, _, Some(vf))) =>
        val ctx = BlockchainContext.build(
          script.version,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        val evalContract = in.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.apply(t)),
                                        _.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.ord(t)), ???))
        EvaluatorV1.evalWithLogging(ctx, evalContract)

      case ContractScript.ContractScriptImpl(_, Contract(_, _, None)) =>
        val t: Proven with Authorized = in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_) => Right(TRUE)
          case Left(_)  => Right(FALSE)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
  }
}
