package com.wavesplatform.transaction.smart.script

import cats.Id
import cats.syntax.either._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang._
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Asset, Expression, StdLibVersion, V3}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR, TRUE}
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
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) =
    applyGeneric(
      in,
      blockchain,
      script,
      isAssetScript,
      scriptContainerAddress,
      complexityLimit,
      default,
      blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls),
      blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls),
      blockchain.height >= blockchain.settings.functionalitySettings.syncDAppCheckPaymentsHeight,
      blockchain.checkEstimatorSumOverflow
    )

  def applyGeneric(
      in: TxOrd,
      blockchain: Blockchain,
      script: Script,
      isAssetScript: Boolean,
      scriptContainerAddress: Environment.Tthis,
      defaultLimit: Int,
      default: EVALUATED,
      useCorrectScriptVersion: Boolean,
      fixUnicodeFunctions: Boolean,
      useNewPowPrecision: Boolean,
      checkEstimatorSumOverflow: Boolean
  ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) = {

    def evalVerifier(
        isContract: Boolean,
        partialEvaluate: (DirectiveSet, EvaluationContext[Environment, Id]) => (Log[Id], Int, Either[ExecutionError, EVALUATED])
    ): (Log[Id], Int, Either[ExecutionError, EVALUATED]) = {
      val txId = in.eliminate(_.id(), _ => ByteStr.empty)
      val ctxE =
        for {
          ds <- DirectiveSet(script.stdLibVersion, if (isAssetScript) Asset else Account, Expression)
          mi <- buildThisValue(in, blockchain, ds, scriptContainerAddress)
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
              txId,
              fixUnicodeFunctions,
              useNewPowPrecision
            )
        } yield (ds, ctx)

      ctxE.fold(e => (Nil, 0, Left(e)), { case (ds, ctx) => partialEvaluate(ds, ctx) })
    }

    def evaluate(ctx: EvaluationContext[Environment, Id], expr: EXPR, version: StdLibVersion): (Log[Id], Int, Either[ExecutionError, EVALUATED]) = {
      val correctedLimit =
        if (isAssetScript)
          ContractLimits.MaxComplexityByVersion(version)
        else
          ContractLimits.MaxAccountVerifierComplexityByVersion(version)

      val (limit, onExceed) =
        if (defaultLimit == Int.MaxValue)
          if (checkEstimatorSumOverflow)
            (correctedLimit, (_: EXPR) => Left(s"Verifier complexity limit = $correctedLimit is exceeded"))
          else
            (defaultLimit, (_: EXPR) => Left(s"Verifier complexity limit = $defaultLimit is exceeded"))
        else
          (defaultLimit, (_: EXPR) => Right(default))

      val (log, unusedComplexity, result) =
        EvaluatorV2.applyOrDefault(ctx, expr, script.stdLibVersion, limit, correctFunctionCallScope = checkEstimatorSumOverflow, onExceed)

      (log, limit - unusedComplexity, result)
    }

    script match {
      case s: ExprScript =>
        evalVerifier(isContract = false, (_, ctx) => evaluate(ctx, s.expr, s.stdLibVersion))

      case ContractScript.ContractScriptImpl(v, DApp(_, decls, _, Some(vf))) =>
        val partialEvaluate: (DirectiveSet, EvaluationContext[Environment, Id]) => (Log[Id], Int, Either[ExecutionError, EVALUATED]) = {
          (directives, ctx) =>
            val verify = ContractEvaluator.verify(decls, vf, evaluate(ctx, _, v), _)
            val bindingsVersion =
              if (useCorrectScriptVersion)
                directives.stdLibVersion
              else
                V3
            in.eliminate(
              t =>
                RealTransactionWrapper(t, blockchain, directives.stdLibVersion, DAppTarget)
                  .fold(e => (Nil, 0, Left(e)), tx => verify(Bindings.transactionObject(tx, proofsEnabled = true, bindingsVersion))),
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
        (Nil, 0, Verifier.verifyAsEllipticCurveSignature(proven).bimap(_.err, _ => TRUE))

      case other =>
        (Nil, 0, s"$other: Unsupported script version".asLeft[EVALUATED])
    }
  }
}
