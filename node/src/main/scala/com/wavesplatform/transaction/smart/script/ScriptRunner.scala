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
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV1, _}
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import com.wavesplatform.transaction.{Authorized, Proven}
import monix.eval.Coeval

object ScriptRunner {
  type TxOrd         = BlockchainContext.In

  def apply(
      in: TxOrd,
      blockchain: Blockchain,
      script: Script,
      isAssetScript: Boolean,
      scriptContainerAddress: ByteStr
  ): (Log[Id], Either[ExecutionError, EVALUATED]) = {
    val evaluate =
      if (blockchain.settings.useEvaluatorV2)
        EvaluatorV2.applyCompleted(_, _, script.stdLibVersion)
      else
        EvaluatorV1().applyWithLogging[EVALUATED] _

    script match {
      case s: ExprScript =>
        val eval = for {
          ds <- DirectiveSet(script.stdLibVersion, if (isAssetScript) Asset else Account, Expression).leftMap((_, Nil))
          mi <- buildThisValue(in, blockchain, ds, Some(scriptContainerAddress)).leftMap((_, Nil))
          ctx <- BlockchainContext.build(
            script.stdLibVersion,
            AddressScheme.current.chainId,
            Coeval.evalOnce(mi),
            Coeval.evalOnce(blockchain.height),
            blockchain,
            isAssetScript,
            isContract = false,
            Coeval(scriptContainerAddress),
            in.eliminate(_.id(), _ => ByteStr.empty)
          ).leftMap((_, Nil))
          result <- evaluate(ctx, s.expr)
        } yield result

        eval.fold(
          { case (error, log)  => (log, error.asLeft[EVALUATED]) },
          { case (result, log) => (log, result.asRight[ExecutionError]) }
        )

      case ContractScript.ContractScriptImpl(_, DApp(_, decls, _, Some(vf))) =>
        val eval = for {
          ds <- DirectiveSet(script.stdLibVersion, Account, Expression).leftMap((_, Nil))
          mi <- buildThisValue(in, blockchain, ds, None).leftMap((_, Nil))
          txId = in.eliminate(_.id(), _ => ByteStr.empty)
          ctx <- BlockchainContext.build(
            script.stdLibVersion,
            AddressScheme.current.chainId,
            Coeval.evalOnce(mi),
            Coeval.evalOnce(blockchain.height),
            blockchain,
            isAssetScript,
            isContract = true,
            Coeval(scriptContainerAddress),
            txId
          ).leftMap((_, Nil))
          verify = ContractEvaluator.verify(decls, vf, ctx, evaluate, _)
          loggedResult <- in.eliminate(
            t => RealTransactionWrapper(t, blockchain, ds.stdLibVersion, DAppTarget)
              .leftMap((_, Nil))
              .flatMap(tx => verify(Bindings.transactionObject(tx, proofsEnabled = true))),
            _.eliminate(
              t => verify(Bindings.orderObject(RealTransactionWrapper.ord(t), proofsEnabled = true)),
              _ => ???
            )
          )
        } yield loggedResult

        eval.fold(
          { case (error, log)  => (log, error.asLeft[EVALUATED]) },
          { case (result, log) => (log, result.asRight[ExecutionError]) }
        )

      case ContractScript.ContractScriptImpl(_, DApp(_, _, _, None)) =>
        val t: Proven with Authorized =
          in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], _ => ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_)                => Right(TRUE)
          case Left(GenericError(err)) => Left(err)
        })
      case other => (List.empty, s"$other: Unsupported script version".asLeft[EVALUATED])
    }
  }
}
