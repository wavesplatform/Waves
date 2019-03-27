package com.wavesplatform.matcher.smart

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV1, Log}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.smart.script.{ContractScript, Script}
import com.wavesplatform.transaction.smart.{RealTransactionWrapper, Verifier}
import com.wavesplatform.transaction.{Authorized, Proven}
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply(script: Script, order: Order, isTokenScript: Boolean): (Log, Either[String, EVALUATED]) = script match {
    case s: ExprScript =>
      val ctx = MatcherContext.build(script.stdLibVersion, AddressScheme.current.chainId, Coeval.evalOnce(order), !isTokenScript)
      EvaluatorV1.applywithLogging(ctx, s.expr)

    case ContractScript.ContractScriptImpl(_, DApp(_, _, Some(vf)), _) =>
      val ctx = MatcherContext.build(
        script.stdLibVersion,
        AddressScheme.current.chainId,
        Coeval.evalOnce(???) /*order not used in global context where @Verifier annotation is used */,
        proofsEnabled = true
      )
      val evalContract = ContractEvaluator.verify(vf, RealTransactionWrapper.ord(order))
      EvaluatorV1.evalWithLogging(ctx, evalContract)

    case ContractScript.ContractScriptImpl(_, DApp(_, _, None), _) =>
      (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](order) match {
        case Right(_) => Right(TRUE)
        case Left(_)  => Right(FALSE)
      })
    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
