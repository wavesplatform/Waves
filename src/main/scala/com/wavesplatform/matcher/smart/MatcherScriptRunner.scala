package com.wavesplatform.matcher.smart

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV1, Log}
import com.wavesplatform.transaction.{Authorized, Proven}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.{RealTransactionWrapper, Verifier}
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.smart.script.v1.ContractScript
import com.wavesplatform.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply(script: Script, order: Order, isTokenScript: Boolean): (Log, Either[String, EVALUATED]) = script match {
    case s: ExprScriprImpl =>
      val ctx = MatcherContext.build(script.version, AddressScheme.current.chainId, Coeval.evalOnce(order), !isTokenScript)
      EvaluatorV1.applywithLogging(ctx, s.expr)

    case ContractScript(_, Contract(_, _, Some(vf))) =>
      val ctx = MatcherContext.build(
        script.version,
        AddressScheme.current.chainId,
        Coeval.evalOnce(???) /*order not used in global context where @Verifier annotation is used */,
        proofsEnabled = true
      )
      val evalContract = ContractEvaluator.verify(vf, RealTransactionWrapper.ord(order))
      EvaluatorV1.evalWithLogging(ctx, evalContract)

    case ContractScript(_, Contract(_, _, None)) =>
      (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](order) match {
        case Right(_) => Right(TRUE)
        case Left(_)  => Right(FALSE)
      })
    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
