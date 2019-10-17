package com.wavesplatform.transaction.smart.script

import cats.Id
import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Asset, Expression}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, TRUE}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, _}
import com.wavesplatform.lang.v1.traits.domain.AttachedPayments
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.smart.{DApp => DAppTarget}
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.{Authorized, Proven, Transaction, smart}
import monix.eval.Coeval
import shapeless._

object ScriptRunner {
  type TxOrd = BlockchainContext.In
  type PaymentsTxOrd = (Transaction, Option[AttachedPayments]) :+: Order :+: ScriptTransfer :+: CNil

  def apply(in: TxOrd,
            blockchain: Blockchain,
            script: Script,
            isAssetScript: Boolean,
            scriptContainerAddress: ByteStr): (Log[Id], Either[ExecutionError, EVALUATED]) =
    script match {
      case s: ExprScript =>
        val evalCtx = for {
          ds  <- DirectiveSet(script.stdLibVersion, if (isAssetScript) Asset else Account, Expression)
          mi  <- buildThisValue(in, blockchain, ds, Some(scriptContainerAddress))
          ctx <- BlockchainContext.build(
            script.stdLibVersion,
            AddressScheme.current.chainId,
            Coeval.evalOnce(mi),
            Coeval.evalOnce(blockchain.height),
            blockchain,
            isAssetScript,
            isContract = false,
            Coeval(scriptContainerAddress)
          )
        } yield ctx
        EvaluatorV1().applyWithLogging[EVALUATED](evalCtx, s.expr)
      case ContractScript.ContractScriptImpl(_, DApp(_, decls, _, Some(vf))) =>
         val r = for {
          ds  <- DirectiveSet(script.stdLibVersion, if (isAssetScript) Asset else Account, Expression)
          mi  <- buildThisValue(in, blockchain, ds, None)
          ctx <- BlockchainContext.build(
            script.stdLibVersion,
            AddressScheme.current.chainId,
            Coeval.evalOnce(mi),
            Coeval.evalOnce(blockchain.height),
            blockchain,
            isAssetScript,
            isContract = true,
            Coeval(scriptContainerAddress)
          )
          entity <- in.eliminate(
            t => RealTransactionWrapper(t, blockchain, ds.stdLibVersion, DAppTarget).map(ContractEvaluator.verify(decls, vf, _)),
            _.eliminate(
              t => ContractEvaluator.verify(decls, vf, RealTransactionWrapper.ord(t)).asRight[ExecutionError],
              _ => ???
            )
          )
        } yield EvaluatorV1().evalWithLogging(ctx, entity)

        r.fold(e => (Nil, e.asLeft[EVALUATED]), identity)

      case ContractScript.ContractScriptImpl(_, DApp(_, _, _, None)) =>
        val t: Proven with Authorized =
          in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], _ => ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_)                => Right(TRUE)
          case Left(GenericError(err)) => Left(err)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
}
