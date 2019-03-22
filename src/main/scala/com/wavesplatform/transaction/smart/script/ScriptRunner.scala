package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, _}
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.smart.{BlockchainContext, RealTransactionWrapper, Verifier}
import com.wavesplatform.transaction.{Authorized, Proven}
import monix.eval.Coeval

object ScriptRunner {
  type TxOrd = BlockchainContext.In

  def apply(height: Int,
            in: TxOrd,
            blockchain: Blockchain,
            script: Script,
            isTokenScript: Boolean,
            dappAddress: Address): (Log, Either[ExecutionError, EVALUATED]) = {
    script match {
      case s: ExprScript =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript,
          false,
          Coeval(dappAddress)
        )
        EvaluatorV1.applywithLogging[EVALUATED](ctx, s.expr)
      case ContractScript.ContractScriptImpl(_, DApp(_, _, Some(vf)), _) =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript,
          true,
          Coeval(dappAddress)
        )
        val evalContract = in.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.apply(t)),
                                        _.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.ord(t)), _ => ???))
        EvaluatorV1.evalWithLogging(ctx, evalContract)

      case ContractScript.ContractScriptImpl(_, DApp(_, _, None), _) =>
        val t: Proven with Authorized =
          in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], _ => ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_) => Right(TRUE)
          case Left(_)  => Right(FALSE)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
  }
}
