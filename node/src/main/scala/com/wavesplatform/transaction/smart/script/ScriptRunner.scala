package com.wavesplatform.transaction.smart.script

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.MultiPaymentPolicyProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Asset, Expression, V4}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, TRUE}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, _}
import com.wavesplatform.lang.v1.traits.domain.Payments
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.{Authorized, Proven, Transaction}
import monix.eval.Coeval
import shapeless.{:+:, CNil, Inl}

object ScriptRunner {
  type TxOrd = BlockchainContext.In
  type PaymentsTxOrd = (Transaction, Option[Payments]) :+: Order :+: ScriptTransfer :+: CNil

  def apply(in: TxOrd,
            blockchain: Blockchain,
            script: Script,
            isAssetScript: Boolean,
            scriptContainerAddress: ByteStr): (Log, Either[ExecutionError, EVALUATED]) =
    script match {
      case s: ExprScript =>
        val paymentCheck = in match {
          case Inl(ist: InvokeScriptTransaction) =>
            val ds = DirectiveSet(s.stdLibVersion, if (isAssetScript) Asset else Account, Expression).explicitGet()
            AttachedPaymentValidator.checkPayments(ist, blockchain.multiPaymentAllowed, ds)
          case _ => Right(())
        }
        lazy val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(blockchain.height),
          blockchain,
          isAssetScript,
          false,
          Coeval(scriptContainerAddress)
        )
        EvaluatorV1.applyWithLogging[EVALUATED](paymentCheck.flatMap(_ => ctx), s.expr)
      case ContractScript.ContractScriptImpl(_, DApp(_, decls, _, Some(vf), version)) =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(blockchain.height),
          blockchain,
          isAssetScript,
          true,
          Coeval(scriptContainerAddress)
        )
        val evalContract = in.eliminate(
          t => ContractEvaluator.verify(decls, vf, RealTransactionWrapper(t, blockchain.multiPaymentAllowed, version)),
          _.eliminate(t => ContractEvaluator.verify(decls, vf, RealTransactionWrapper.ord(t)), _ => ???)
        )
        EvaluatorV1.evalWithLogging(ctx, evalContract)

      case ContractScript.ContractScriptImpl(_, DApp(_, _, _, None, _)) =>
        val t: Proven with Authorized =
          in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], _ => ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_)                => Right(TRUE)
          case Left(GenericError(err)) => Left(err)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
}
