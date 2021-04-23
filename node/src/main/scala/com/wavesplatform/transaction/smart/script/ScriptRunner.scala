package com.wavesplatform.transaction.smart.script

import cats.Id
import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Asset, Expression, V3}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, TRUE}
import com.wavesplatform.lang.v1.evaluator._
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Bindings
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import com.wavesplatform.transaction.{Authorized, Proven}
import monix.eval.Coeval

object ScriptRunner {
  type TxOrd = BlockchainContext.In

  def apply(
      in: TxOrd,
      blockchain: Blockchain,
      script: Script,
      isAssetScript: Boolean,
      scriptContainerAddress: Environment.Tthis,
      complexityLimit: Int = Int.MaxValue,
      default: EVALUATED = TRUE
  ): (Log[Id], Either[ExecutionError, EVALUATED]) =
    applyGeneric(
      in,
      blockchain,
      script,
      isAssetScript,
      scriptContainerAddress,
      complexityLimit,
      default,
      blockchain.isFeatureActivated(BlockchainFeatures.ContinuationTransaction)
    )

  def applyGeneric(
      in: TxOrd,
      blockchain: Blockchain,
      script: Script,
      isAssetScript: Boolean,
      scriptContainerAddress: Environment.Tthis,
      complexityLimit: Int,
      default: EVALUATED,
      useCorrectScriptVersion: Boolean
  ): (Log[Id], Either[ExecutionError, EVALUATED]) = {

    def evalVerifier(
        isContract: Boolean,
        partialEvaluate: (DirectiveSet, EvaluationContext[Environment, Id]) => Either[(ExecutionError, Log[Id]), (EVALUATED, Log[Id])]
    ) = {
      val txId = in.eliminate(_.id(), _ => ByteStr.empty)
      val eval = for {
        ds <- DirectiveSet(script.stdLibVersion, if (isAssetScript) Asset else Account, Expression).leftMap((_, Nil))
        mi <- buildThisValue(in, blockchain, ds, scriptContainerAddress).leftMap((_, Nil))
        ctx <- BlockchainContext
          .build(
            script.stdLibVersion,
            AddressScheme.current.chainId,
            Coeval.evalOnce(mi),
            Coeval.evalOnce(blockchain.height),
            blockchain,
            isAssetScript,
            isContract,
            scriptContainerAddress,
            txId
          )
          .leftMap((_, Nil))
        result <- partialEvaluate(ds, ctx)
      } yield result

      eval.fold(
        { case (error, log)  => (log, error.asLeft[EVALUATED]) },
        { case (result, log) => (log, result.asRight[ExecutionError]) }
      )
    }

    val evaluate =
      if (complexityLimit == Int.MaxValue)
        EvaluatorV2.applyCompleted(_, _, script.stdLibVersion)
      else
        EvaluatorV2.applyOrDefault(_, _, script.stdLibVersion, complexityLimit, default)

    script match {
      case s: ExprScript =>
        evalVerifier(isContract = false, (_, ctx) => evaluate(ctx, s.expr))

      case ContractScript.ContractScriptImpl(_, DApp(_, decls, _, Some(vf))) =>
        val partialEvaluate: (DirectiveSet, EvaluationContext[Environment, Id]) => Either[(ExecutionError, Log[Id]), (EVALUATED, Log[Id])] = {
          (directives, ctx) =>
            val verify = ContractEvaluator.verify(decls, vf, ctx, evaluate, _)
            val bindingsVersion =
              if (useCorrectScriptVersion)
                directives.stdLibVersion
              else
                V3
            in.eliminate(
              t =>
                RealTransactionWrapper(t, blockchain, directives.stdLibVersion, DAppTarget)
                  .leftMap((_, Nil))
                  .flatMap(tx => verify(Bindings.transactionObject(tx, proofsEnabled = true, bindingsVersion))),
              _.eliminate(
                t => verify(Bindings.orderObject(RealTransactionWrapper.ord(t), proofsEnabled = true)),
                _ => ???
              )
            )
        }
        evalVerifier(isContract = true, partialEvaluate)

      case ContractScript.ContractScriptImpl(_, DApp(_, _, _, None)) =>
        val proven: Proven with Authorized =
          in.eliminate(
            _.asInstanceOf[Proven with Authorized],
            _.eliminate(
              _.asInstanceOf[Proven with Authorized],
              _ => ???
            )
          )
        (Nil, Verifier.verifyAsEllipticCurveSignature(proven).bimap(_.err, _ => TRUE))

      case other =>
        (Nil, s"$other: Unsupported script version".asLeft[EVALUATED])
    }
  }
}
