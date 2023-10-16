package com.wavesplatform.api.http.utils

import cats.Id
import cats.implicits.catsSyntaxSemigroup
import cats.syntax.either.*
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.features.EvaluatorFixProvider.*
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.compiler.{ContractScriptCompactor, ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.{Invocation, LogExtraInfo}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV2, Log, ScriptResult}
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.lang.v1.traits.Environment.Tthis
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.lang.{ValidationError, utils}
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, ScriptExtraFee}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.diffs.invoke.{InvokeDiffsCommon, InvokeScriptTransactionLike, StructuredCallableActions}
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.{Blockchain, InvokeScriptResult, Portfolio, StateSnapshot}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TransactionType.{InvokeScript, TransactionType}
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvokeRejectError}
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.smart.DAppEnvironment.ActionLimits
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.validation.impl.InvokeScriptTxValidator
import com.wavesplatform.transaction.{Asset, TransactionType}
import monix.eval.Coeval
import shapeless.Coproduct

object UtilsEvaluator {
  def compile(version: StdLibVersion)(str: String): Either[GenericError, EXPR] =
    ExpressionCompiler
      .compileUntyped(str, NoLibraries, utils.compilerContext(version, Expression, isAssetScript = false).copy(arbitraryDeclarations = true), version)
      .leftMap(GenericError(_))

  def toInvokeScriptLike(invocation: Invocation, dAppAddress: Address) =
    new InvokeScriptTransactionLike {
      override def dApp: AddressOrAlias              = dAppAddress
      override def funcCall: Terms.FUNCTION_CALL     = invocation.funcCall
      override def root: InvokeScriptTransactionLike = this
      override val sender: PublicKey                 = PublicKey(invocation.callerPk)
      override def assetFee: (Asset, Long)           = (Asset.fromCompatId(invocation.feeAssetId), invocation.fee)
      override def timestamp: Long                   = System.currentTimeMillis()
      override def chainId: Byte                     = AddressScheme.current.chainId
      override def id: Coeval[ByteStr]               = Coeval(invocation.transactionId)
      override val tpe: TransactionType              = TransactionType.InvokeScript
      override def payments: Seq[Payment] =
        invocation.payments.payments.map { case (amount, assetId) =>
          Payment(amount, Asset.fromCompatId(assetId))
        }
    }

  def emptyInvokeScriptLike(dAppAddress: Address) =
    new InvokeScriptTransactionLike {
      override def dApp: AddressOrAlias              = dAppAddress
      override def funcCall: Terms.FUNCTION_CALL     = Terms.FUNCTION_CALL(FunctionHeader.User(""), Nil)
      override def payments: Seq[Payment]            = Seq.empty
      override def root: InvokeScriptTransactionLike = this
      override val sender: PublicKey                 = PublicKey(ByteStr(new Array[Byte](32)))
      override def assetFee: (Asset, Long)           = Asset.Waves -> FeeConstants(InvokeScript) * ScriptExtraFee
      override def timestamp: Long                   = System.currentTimeMillis()
      override def chainId: Byte                     = AddressScheme.current.chainId
      override def id: Coeval[ByteStr]               = Coeval.evalOnce(ByteStr.empty)
      override val tpe: TransactionType              = TransactionType.InvokeScript
    }

  def executeExpression(blockchain: Blockchain, script: Script, dAppAddress: Address, dAppPk: PublicKey, limit: Int)(
      invoke: InvokeScriptTransactionLike,
      dAppToExpr: DApp => Either[ValidationError, EXPR]
  ): Either[ValidationError, (EVALUATED, Int, Log[Id], InvokeScriptResult)] =
    for {
      _                <- InvokeScriptTxValidator.checkAmounts(invoke.payments).toEither.leftMap(_.head)
      ds               <- DirectiveSet(script.stdLibVersion, Account, DAppType).leftMap(GenericError(_))
      paymentsSnapshot <- InvokeDiffsCommon.paymentsPart(blockchain, invoke, dAppAddress, Map())
      environment = new DAppEnvironment(
        AddressScheme.current.chainId,
        Coeval.raiseError(new IllegalStateException("No input entity available")),
        Coeval.evalOnce(blockchain.height),
        blockchain,
        Coproduct[Tthis](Recipient.Address(ByteStr(dAppAddress.bytes))),
        ds,
        script.stdLibVersion,
        invoke,
        dAppAddress,
        dAppPk,
        Set.empty[Address],
        limitedExecution = false,
        enableExecutionLog = true,
        limit,
        remainingCalls = ContractLimits.MaxSyncDAppCalls(script.stdLibVersion),
        availableActions = ActionLimits(
          ContractLimits.MaxCallableActionsAmountBeforeV6(script.stdLibVersion),
          ContractLimits.MaxBalanceScriptActionsAmountV6,
          ContractLimits.MaxAssetScriptActionsAmountV6,
          ContractLimits.MaxWriteSetSize,
          ContractLimits.MaxTotalWriteSetSizeInBytes
        ),
        availablePayments = ContractLimits.MaxTotalPaymentAmountRideV6,
        currentSnapshot = paymentsSnapshot,
        invocationRoot = DAppEnvironment.InvocationTreeTracker(DAppEnvironment.DAppInvocation(dAppAddress, null, Nil))
      )
      ctx  = BlockchainContext.build(ds, environment, fixUnicodeFunctions = true, useNewPowPrecision = true, fixBigScriptField = true, typedError = true)
      dApp = ContractScriptCompactor.decompact(script.expr.asInstanceOf[DApp])
      expr <- dAppToExpr(dApp)
      limitedResult <- EvaluatorV2
        .applyLimitedCoeval(
          expr,
          LogExtraInfo(),
          limit,
          ctx,
          script.stdLibVersion,
          correctFunctionCallScope = blockchain.checkEstimatorSumOverflow,
          newMode = blockchain.newEvaluatorMode,
          checkConstructorArgsTypes = true,
          enableExecutionLog = true
        )
        .value()
        .leftMap { case (err, _, log) => InvokeRejectError(err.message, log) }
      (evaluated, usedComplexity, log) <- limitedResult match {
        case (eval: EVALUATED, unusedComplexity, log) => Right((eval, limit - unusedComplexity, log))
        case (_: EXPR, _, log)                        => Left(InvokeRejectError(s"Calculation complexity limit exceeded", log))
      }
      snapshot <- ScriptResult
        .fromObj(ctx, invoke.id(), evaluated, ds.stdLibVersion, unusedComplexity = 0)
        .bimap(
          _ => Right(StateSnapshot.empty),
          r =>
            InvokeDiffsCommon
              .processActions(
                StructuredCallableActions(r.actions, blockchain),
                ds.stdLibVersion,
                script.stdLibVersion,
                dAppAddress,
                dAppPk,
                usedComplexity,
                invoke,
                SnapshotBlockchain(blockchain, paymentsSnapshot),
                System.currentTimeMillis(),
                isSyncCall = false,
                limitedExecution = false,
                limit,
                Nil,
                enableExecutionLog = true,
                log
              )
              .resultE
        )
        .merge
      totalDiff     = snapshot |+| paymentsSnapshot
      totalSnapshot = addWavesToDefaultInvoker(totalDiff, blockchain)
      _ <- TransactionDiffer.validateBalance(blockchain, InvokeScript, totalSnapshot)
      _ <- TransactionDiffer.assetsVerifierDiff(blockchain, invoke, verify = true, totalSnapshot, Int.MaxValue, enableExecutionLog = true).resultE
      rootScriptResult  = snapshot.scriptResults.headOption.map(_._2).getOrElse(InvokeScriptResult.empty)
      innerScriptResult = environment.currentSnapshot.scriptResults.values.fold(InvokeScriptResult.empty)(_ |+| _)
    } yield (evaluated, usedComplexity, log, innerScriptResult |+| rootScriptResult)

  private def addWavesToDefaultInvoker(snapshot: StateSnapshot, blockchain: Blockchain) =
    if (snapshot.balances.get((UtilsApiRoute.DefaultAddress, Waves)).exists(_ >= Long.MaxValue / 10))
      snapshot
    else
      snapshot.addBalances(Map(UtilsApiRoute.DefaultAddress -> Portfolio.waves(Long.MaxValue / 10)), blockchain).explicitGet()
}
