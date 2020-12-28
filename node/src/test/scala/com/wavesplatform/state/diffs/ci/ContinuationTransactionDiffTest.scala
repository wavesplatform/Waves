package com.wavesplatform.state.diffs.ci

import cats.data.Ior
import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp, V5}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV2, IncompleteResult}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{Burn, Recipient, Reissue}
import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.settings.TestSettings
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.state.diffs.invoke.{ContinuationTransactionDiff, InvokeScriptTransactionDiff}
import com.wavesplatform.transaction.ApplicationStatus.{ScriptExecutionInProgress, Succeeded}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.FailedTransactionError.Cause
import com.wavesplatform.transaction.TxValidationError.{FailedTransactionError, ScriptExecutionError}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{Inside, Matchers, PropSpec}
import shapeless.Coproduct

import scala.util.Random

class ContinuationTransactionDiffTest extends PropSpec with PathMockFactory with TransactionGen with Matchers with Inside {
  private val transferAddress = accountGen.sample.get.toAddress
  private val dAppPk          = accountGen.sample.get.publicKey
  private val scriptedAssetId = bytes32gen.map(ByteStr(_)).sample.get
  private val failingAssetId  = bytes32gen.map(ByteStr(_)).sample.get
  private val scriptedAsset   = IssuedAsset(scriptedAssetId)
  private val failingAsset    = IssuedAsset(failingAssetId)

  private val paymentAmount  = 7L
  private val transferAmount = 10L
  private val reissueAmount  = 100L
  private val burnAmount     = 50L
  private val timestamp      = timestampGen.sample.get

  private val assetScriptComplexity = 123
  private val invokeGen             = invokeScriptGen(Gen.const(Seq(Payment(paymentAmount, scriptedAsset))))

  private val dApp =
    compile(
      s"""
        | {-# STDLIB_VERSION 5 #-}
        | {-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(i)
        | func oneStepExpr() = {
        |   let a = !(${List.fill(10)("sigVerify(base64'', base64'', base64'')").mkString("||")})
        |   if (a)
        |     then
        |       [
        |         BooleanEntry("isAllowed", true),
        |         Reissue(base58'$scriptedAssetId', $reissueAmount, true),
        |         Burn(base58'$scriptedAssetId', $burnAmount),
        |         ScriptTransfer(Address(base58'${transferAddress.stringRepr}'), $transferAmount, base58'$scriptedAssetId')
        |       ]
        |     else
        |       throw("unexpected")
        | }
        |
        | @Callable(i)
        | func failingExpr() = {
        |   let a = !(${List.fill(10)("sigVerify(base64'', base64'', base64'')").mkString("||")})
        |   if (a)
        |     then
        |       throw("failed")
        |     else
        |       throw("unexpected")
        | }
        |
        | @Callable(i)
        | func failingAssetVerifier() = {
        |   let a = !(${List.fill(10)("sigVerify(base64'', base64'', base64'')").mkString("||")})
        |   if (a)
        |     then
        |       [
        |         BooleanEntry("isAllowed", true),
        |         Reissue(base58'$scriptedAssetId', $reissueAmount, true),
        |         Burn(base58'$scriptedAssetId', $burnAmount),
        |         ScriptTransfer(Address(base58'${transferAddress.stringRepr}'), $transferAmount, base58'$failingAssetId')
        |       ]
        |     else
        |       throw("unexpected")
        | }
        |
        |
        | @Callable(i)
        | func multiStepExpr() = {
        |   let a = !(${List.fill(100)("sigVerify(base64'', base64'', base64'')").mkString("||")})
        |   if (a)
        |     then
        |       [BooleanEntry("isAllowed", true)]
        |     else
        |       throw("unexpected")
        | }
      """.stripMargin
    ).asInstanceOf[ContractScriptImpl]

  private def compile(scriptText: String): Script =
    ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()._1

  private def blockchainMock(invoke: InvokeScriptTransaction, func: (String, Long), exprInfo: Option[(Int, EXPR, Int)]) = {
    val blockchain = mock[Blockchain]
    exprInfo.foreach {
      case (step, expr, unusedComplexity) =>
        (() => blockchain.continuationStates)
          .expects()
          .anyNumberOfTimes()
          .returning(
            Map(
              (
                invoke.dAppAddressOrAlias.asInstanceOf[Address],
                ContinuationState.InProgress(expr, unusedComplexity, invoke.id.value(), step)
              )
            )
          )
    }
    (blockchain.transactionInfo _)
      .expects(invoke.id.value())
      .returning(Some((1, invoke, Succeeded)))
      .anyNumberOfTimes()
    (blockchain.transactionMeta _)
      .expects(invoke.id.value())
      .returning(Some((1, Succeeded)))
      .anyNumberOfTimes()
    (blockchain.accountScript _)
      .expects(invoke.dAppAddressOrAlias.asInstanceOf[Address])
      .returning(Some(AccountScriptInfo(invoke.sender, dApp, 99L, Map(3 -> Map(func))))) // verifier complexity counts separately in Verifier.scala
    (() => blockchain.activatedFeatures)
      .expects()
      .returning(Map(BlockchainFeatures.Ride4DApps.id -> 0, BlockchainFeatures.BlockV5.id -> 0))
      .anyNumberOfTimes()
    (() => blockchain.height)
      .expects()
      .returning(1)
      .anyNumberOfTimes()
    (blockchain.assetScript _)
      .expects(failingAsset)
      .returning(
        Some(
          AssetScriptInfo(
            compile(""" if (true) then throw("failed by asset verifier") else false """),
            assetScriptComplexity
          )
        )
      )
      .anyNumberOfTimes()
    (blockchain.assetScript _)
      .expects(*)
      .returning(Some(AssetScriptInfo(compile("true"), assetScriptComplexity)))
      .anyNumberOfTimes()
    (blockchain.assetDescription _)
      .expects(failingAsset)
      .returning(
        Some(
          AssetDescription(
            ByteStr.empty,
            dAppPk,
            ByteString.EMPTY,
            ByteString.EMPTY,
            1,
            true,
            BigInt(Int.MaxValue),
            Height(1),
            Some(AssetScriptInfo(compile(""" if (true) then throw("failed by asset verifier") else false """), assetScriptComplexity)),
            0,
            false
          )
        )
      )
      .anyNumberOfTimes()
    (blockchain.assetDescription _)
      .expects(*)
      .returning(
        Some(
          AssetDescription(
            ByteStr.empty,
            dAppPk,
            ByteString.EMPTY,
            ByteString.EMPTY,
            1,
            true,
            BigInt(Int.MaxValue),
            Height(1),
            Some(AssetScriptInfo(compile("true"), assetScriptComplexity)),
            0,
            false
          )
        )
      )
      .anyNumberOfTimes()
    (blockchain.hasAccountScript _)
      .expects(invoke.sender.toAddress)
      .returning(true)
      .anyNumberOfTimes()
    (() => blockchain.settings)
      .expects()
      .returning(TestSettings.Default.blockchainSettings)
      .anyNumberOfTimes()
    (blockchain.blockHeader _)
      .expects(*)
      .returning(
        Some(
          SignedBlockHeader(
            BlockHeader(1, 1, ByteStr.empty, 1, ByteStr.empty, PublicKey(new Array[Byte](32)), Seq(), 1, ByteStr.empty),
            ByteStr.empty
          )
        )
      )
      .anyNumberOfTimes()
    blockchain
  }

  private def evaluateContinuationStep(expr: EXPR, unusedComplexity: Int): (EXPR, Int) = {
    val ds        = DirectiveSet(V5, Account, DApp).explicitGet()
    val wavesCtx  = WavesContext.build(ds)
    val cryptoCtx = CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment]
    val pureCtx   = PureContext.build(ds.stdLibVersion).withEnvironment[Environment]
    val ctx       = (pureCtx |+| cryptoCtx |+| wavesCtx).evaluationContext(Common.emptyBlockchainEnvironment())
    val (resultExpr, unused, _) =
      EvaluatorV2
        .applyLimited(expr, ContractLimits.MaxComplexityByVersion(V5) + unusedComplexity, ctx, V5, continuationFirstStepMode = true)
        .explicitGet()
    (resultExpr, unused)
  }

  private def evaluateInvokeFirstStep(invoke: InvokeScriptTransaction, blockchain: Blockchain): (EXPR, Int) = {
    val ds        = DirectiveSet(V5, Account, DApp).explicitGet()
    val wavesCtx  = WavesContext.build(ds)
    val cryptoCtx = CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment]
    val pureCtx   = PureContext.build(ds.stdLibVersion).withEnvironment[Environment]
    val ctx       = (pureCtx |+| cryptoCtx |+| wavesCtx).evaluationContext(Common.emptyBlockchainEnvironment())
    val invocation = ContractEvaluator.Invocation(
      invoke.funcCall,
      Recipient.Address(ByteStr(invoke.sender.toAddress.bytes)),
      invoke.sender,
      AttachedPaymentExtractor.extractPayments(invoke, V5, blockchain, DAppTarget).explicitGet(),
      invoke.id.value(),
      invoke.fee,
      invoke.feeAssetId.compatId
    )
    val environment = new WavesEnvironment(
      AddressScheme.current.chainId,
      Coeval(???),
      Coeval(1),
      blockchain,
      Coproduct[Environment.Tthis](Recipient.Address(ByteStr(invoke.dAppAddressOrAlias.bytes))),
      DirectiveSet.contractDirectiveSet,
      ByteStr.empty
    )
    val IncompleteResult(resultExpr, unusedComplexity) = ContractEvaluator
      .applyV2(
        ctx,
        wavesCtx.evaluationContext(environment).letDefs,
        dApp.expr,
        invocation,
        V5,
        ContractLimits.MaxComplexityByVersion(V5),
        continuationFirstStepMode = true
      )
      .explicitGet()
      ._1
      .asInstanceOf[IncompleteResult]
    (resultExpr, unusedComplexity)
  }

  property("continuation in progress result after invoke") {
    val invoke      = invokeGen.sample.get.copy(funcCallOpt = Some(FUNCTION_CALL(User("multiStepExpr"), Nil)))
    val dAppAddress = invoke.dAppAddressOrAlias.asInstanceOf[Address]
    val blockchain  = blockchainMock(invoke, ("multiStepExpr", 1234L), None)

    val (resultExpr, unusedComplexity) = evaluateInvokeFirstStep(invoke, blockchain)
    val sigVerifyComplexity            = 200
    unusedComplexity should be < sigVerifyComplexity
    val spentComplexity = ContractLimits.MaxComplexityByVersion(V5) - unusedComplexity

    InvokeScriptTransactionDiff(blockchain, invoke.timestamp, false)(invoke).resultE shouldBe Right(
      Diff.empty.copy(
        transactions = Map(invoke.id.value() -> NewTransactionInfo(invoke, Set(), ScriptExecutionInProgress)),
        portfolios = Map(
          invoke.sender.toAddress -> Portfolio(-invoke.fee, assets = Map(scriptedAsset -> -paymentAmount)),
          dAppAddress             -> Portfolio.build(scriptedAsset, paymentAmount)
        ),
        continuationStates = Map(
          (dAppAddress, ContinuationState.InProgress(resultExpr, unusedComplexity, invoke.id.value(), 0))
        ),
        scriptsRun = 3, // step, payment and account script
        scriptsComplexity = spentComplexity
      )
    )
    Verifier.assets(blockchain, Int.MaxValue)(invoke).resultE shouldBe
      Right(Diff.empty.copy(scriptsComplexity = assetScriptComplexity))
  }

  property("continuation in progress result after continuation") {
    val expr                             = dApp.expr.callableFuncs.find(_.u.name == "multiStepExpr").get.u.body
    val precedingStepCount               = Random.nextInt(Int.MaxValue)
    val invoke                           = invokeGen.sample.get.copy(funcCallOpt = Some(FUNCTION_CALL(User("multiStepExpr"), Nil)))
    val continuation                     = ContinuationTransaction(invoke.id.value(), precedingStepCount, fee = 0L, invoke.feeAssetId, timestamp)
    val stepFee                          = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit
    val startUnusedComplexity            = Random.nextInt(2000)
    val (result, resultUnusedComplexity) = evaluateContinuationStep(expr, startUnusedComplexity)
    val blockchain                       = blockchainMock(invoke, ("multiStepExpr", 1234L), Some((precedingStepCount, expr, startUnusedComplexity)))

    val sigVerifyComplexity = 200
    resultUnusedComplexity should be < sigVerifyComplexity
    val spentComplexity = ContractLimits.MaxComplexityByVersion(V5) + startUnusedComplexity - resultUnusedComplexity

    val state = ContinuationState.InProgress(result, resultUnusedComplexity, invoke.id.value(), precedingStepCount + 1)
    ContinuationTransactionDiff(blockchain, continuation.timestamp, false)(continuation).resultE shouldBe
      Right(
        Diff.empty.copy(
          replacingTransactions = Seq(NewTransactionInfo(continuation.copy(fee = stepFee), Set(), Succeeded)),
          scriptsRun = 1,
          scriptsComplexity = spentComplexity,
          continuationStates = Map(invoke.dAppAddressOrAlias.asInstanceOf[Address] -> state)
        )
      )
  }

  property("continuation finish result with scripted actions and payment") {
    val dAppAddress             = dAppPk.toAddress
    val actionScriptInvocations = 3
    val precedingStepCount      = 2 // 4 steps: invoke and 3 continuations (0, 1, 2) were completed
    val actionFee               = ScriptExtraFee * actionScriptInvocations
    val stepFee                 = (precedingStepCount + 2) * FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit + actionFee
    val func                    = Some(FUNCTION_CALL(User("oneStepExpr"), Nil))
    val invoke                  = invokeGen.sample.get.copy(funcCallOpt = func, dAppAddressOrAlias = dAppAddress)
    val continuation            = ContinuationTransaction(invoke.id.value(), precedingStepCount, fee = 0L, invoke.feeAssetId, timestamp)
    val expr                    = dApp.expr.callableFuncs.find(_.u.name == "oneStepExpr").get.u.body

    val actualComplexity    = 2018
    val estimatedComplexity = actualComplexity + Random.nextInt(1000)
    val blockchain          = blockchainMock(invoke, ("oneStepExpr", estimatedComplexity), Some((precedingStepCount, expr, 0)))

    ContinuationTransactionDiff(blockchain, continuation.timestamp, false)(continuation).resultE shouldBe
      Right(
        Diff.empty.copy(
          portfolios = Map(
            invoke.sender.toAddress -> Portfolio(invoke.fee - stepFee),
            dAppAddress             -> Portfolio.build(scriptedAsset, reissueAmount - burnAmount - transferAmount),
            transferAddress         -> Portfolio.build(scriptedAsset, transferAmount)
          ),
          accountData = Map(dAppAddress -> AccountDataInfo(Map("isAllowed" -> BooleanDataEntry("isAllowed", true)))),
          scriptsRun = actionScriptInvocations + 1, // with step
          scriptResults = Map(
            invoke.id.value() -> InvokeScriptResult(
              data = Seq(BooleanDataEntry("isAllowed", true)),
              transfers = Seq(InvokeScriptResult.Payment(transferAddress, scriptedAsset, transferAmount)),
              reissues = Seq(Reissue(scriptedAssetId, true, reissueAmount)),
              burns = Seq(Burn(scriptedAssetId, burnAmount))
            )
          ),
          scriptsComplexity = actualComplexity + actionScriptInvocations * assetScriptComplexity,
          updatedAssets = Map(scriptedAsset -> Ior.Right(AssetVolumeInfo(true, reissueAmount - burnAmount))),
          replacingTransactions = Seq(
            NewTransactionInfo(continuation.copy(fee = stepFee), Set(), Succeeded),
            NewTransactionInfo(invoke, Set(dAppAddress, transferAddress), Succeeded)
          ),
          continuationStates = Map(invoke.dAppAddressOrAlias.asInstanceOf[Address] -> ContinuationState.Finished)
        )
      )
  }

  property("failed continuation") {
    val dAppAddress             = dAppPk.toAddress
    val actionScriptInvocations = 3
    val stepFee                 = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit + ScriptExtraFee * (actionScriptInvocations + 1)
    val invoke = invokeGen.sample.get.copy(
      funcCallOpt = Some(FUNCTION_CALL(User("failingExpr"), List(CONST_BOOLEAN(true)))),
      fee = stepFee + ScriptExtraFee, // for account script
      dAppAddressOrAlias = dAppAddress
    )
    val precedingStepCount = Random.nextInt(Int.MaxValue)
    val continuation       = ContinuationTransaction(invoke.id.value(), precedingStepCount, fee = 0L, invoke.feeAssetId, timestamp)
    val expr               = dApp.expr.callableFuncs.find(_.u.name == "failingExpr").get.u.body

    val actualComplexity    = 2018
    val estimatedComplexity = actualComplexity + Random.nextInt(1000)
    val blockchain          = blockchainMock(invoke, ("failingExpr", estimatedComplexity), Some((precedingStepCount, expr, 0)))
    val stepLimit           = 4000

    inside(ContinuationTransactionDiff(blockchain, continuation.timestamp, false)(continuation).resultE) {
      case Left(
          FailedTransactionError(Cause.DAppExecution, `stepLimit`, _, Some("failed"), None)
          ) =>
    }
  }

  property("failed by action asset verifier continuation") {
    val dAppAddress             = dAppPk.toAddress
    val actionScriptInvocations = 3
    val stepFee                 = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit + ScriptExtraFee * (actionScriptInvocations + 1)
    val invoke = invokeGen.sample.get.copy(
      funcCallOpt = Some(FUNCTION_CALL(User("failingAssetVerifier"), List(CONST_BOOLEAN(true)))),
      fee = stepFee + ScriptExtraFee, // for account script
      dAppAddressOrAlias = dAppAddress
    )
    val precedingStepCount = Random.nextInt(Int.MaxValue)
    val continuation       = ContinuationTransaction(invoke.id.value(), precedingStepCount, fee = 0L, invoke.feeAssetId, timestamp)
    val expr               = dApp.expr.callableFuncs.find(_.u.name == "failingAssetVerifier").get.u.body

    val actualComplexity    = 2018
    val estimatedComplexity = actualComplexity + Random.nextInt(1000)
    val blockchain          = blockchainMock(invoke, ("failingAssetVerifier", estimatedComplexity), Some((precedingStepCount, expr, 0)))

    val expectingComplexity = actualComplexity + assetScriptComplexity * 3
    inside(ContinuationTransactionDiff(blockchain, continuation.timestamp, false)(continuation).resultE) {
      case Left(
          FailedTransactionError(Cause.AssetScriptInAction, `expectingComplexity`, _, Some("failed by asset verifier"), Some(`failingAssetId`))
          ) =>
    }
  }

  property("failed by payment asset verifier") {
    val invoke             = invokeScriptGen(Gen.const(Seq(Payment(paymentAmount, failingAsset)))).sample.get
    val precedingStepCount = Random.nextInt(Int.MaxValue)
    val continuation       = ContinuationTransaction(invoke.id.value(), precedingStepCount, fee = 0L, invoke.feeAssetId, timestamp)
    val expr               = dApp.expr.callableFuncs.find(_.u.name == "oneStepExpr").get.u.body
    val blockchain         = blockchainMock(invoke, ("oneStepExpr", 123), Some((precedingStepCount, expr, 0)))

    Verifier.assets(blockchain, Int.MaxValue)(continuation).resultE shouldBe Right(Diff.empty)
    Verifier.assets(blockchain, Int.MaxValue)(invoke).resultE shouldBe
      Left((assetScriptComplexity, ScriptExecutionError("failed by asset verifier", Nil, Some(`failingAssetId`))))
  }
}
