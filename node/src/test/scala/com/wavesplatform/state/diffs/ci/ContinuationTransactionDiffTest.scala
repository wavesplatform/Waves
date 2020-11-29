package com.wavesplatform.state.diffs.ci
import cats.implicits._
import com.wavesplatform.TransactionGen
import com.wavesplatform.account.{Address, AddressScheme, PublicKey}
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp, V4}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV2, IncompleteResult}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.settings.TestSettings
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.state.diffs.invoke.{ContinuationTransactionDiff, InvokeScriptTransactionDiff}
import com.wavesplatform.transaction.ApplicationStatus.{ScriptExecutionInProgress, Succeeded}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{DApp => DAppTarget, _}
import monix.eval.Coeval
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{Matchers, PropSpec}
import shapeless.Coproduct

import scala.util.Random

class ContinuationTransactionDiffTest extends PropSpec with PathMockFactory with TransactionGen with Matchers {
  private val dApp =
    compile(
      s"""
        | {-# STDLIB_VERSION 4 #-}
        | {-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(i)
        | func oneStepExpr() = {
        |   let a = !(${List.fill(10)("sigVerify(base64'',base64'',base64'')").mkString("||")})
        |   if (a)
        |     then
        |       [BooleanEntry("isAllowed", true)]
        |     else
        |       throw("unexpected")
        | }
        |
        | @Callable(i)
        | func multiStepExpr() = {
        |   let a = !(${List.fill(100)("sigVerify(base64'',base64'',base64'')").mkString("||")})
        |   if (a)
        |     then
        |       [BooleanEntry("isAllowed", true)]
        |     else
        |       throw("unexpected")
        | }
      """.stripMargin
    )

  private lazy val dummyEstimator = new ScriptEstimator {
    override val version: Int = 0
    override def apply(
        declaredVals: Set[String],
        functionCosts: Map[FunctionHeader, Coeval[Long]],
        expr: Terms.EXPR
    ): Either[String, Long] = Right(1)
  }

  private def compile(scriptText: String): ContractScriptImpl =
    ScriptCompiler
      .compile(scriptText, dummyEstimator)
      .explicitGet()
      ._1
      .asInstanceOf[ContractScriptImpl]

  private def blockchainMock(invoke: InvokeScriptTransaction, func: (String, Long), exprInfo: Option[(Int, EXPR, Int)]) = {
    val blockchain = mock[Blockchain]
    exprInfo.foreach {
      case (step, expr, unusedComplexity) =>
        (() => blockchain.continuationStates)
          .expects()
          .returning(Map((invoke.id.value(), (step, ContinuationState.InProgress(expr, unusedComplexity)))))
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
      .returning(Some(AccountScriptInfo(invoke.sender, dApp, 99L, Map(3 -> Map(func)))))
    (() => blockchain.activatedFeatures)
      .expects()
      .returning(Map(BlockchainFeatures.Ride4DApps.id -> 0, BlockchainFeatures.BlockV5.id -> 0))
      .anyNumberOfTimes()
    (() => blockchain.height)
      .expects()
      .returning(1)
      .anyNumberOfTimes()
    (blockchain.assetScript _)
      .expects(*)
      .returning(None)
      .anyNumberOfTimes()
    (blockchain.hasAccountScript _)
      .expects(invoke.sender.toAddress)
      .returning(false)
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
    val ds        = DirectiveSet(V4, Account, DApp).explicitGet()
    val wavesCtx  = WavesContext.build(ds)
    val cryptoCtx = CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment]
    val pureCtx   = PureContext.build(ds.stdLibVersion).withEnvironment[Environment]
    val ctx       = (pureCtx |+| cryptoCtx |+| wavesCtx).evaluationContext(Common.emptyBlockchainEnvironment())
    val (resultExpr, unused, _) =
      EvaluatorV2
        .applyLimited(expr, ContractLimits.MaxComplexityByVersion(V4) + unusedComplexity, ctx, V4, continuationFirstStepMode = true)
        .explicitGet()
    (resultExpr, unused)
  }

  private def evaluateInvokeFirstStep(invoke: InvokeScriptTransaction, blockchain: Blockchain): (EXPR, Int) = {
    val ds        = DirectiveSet(V4, Account, DApp).explicitGet()
    val wavesCtx  = WavesContext.build(ds)
    val cryptoCtx = CryptoContext.build(Global, ds.stdLibVersion).withEnvironment[Environment]
    val pureCtx   = PureContext.build(ds.stdLibVersion).withEnvironment[Environment]
    val ctx       = (pureCtx |+| cryptoCtx |+| wavesCtx).evaluationContext(Common.emptyBlockchainEnvironment())
    val invocation = ContractEvaluator.Invocation(
      invoke.funcCall,
      Recipient.Address(ByteStr(invoke.sender.toAddress.bytes)),
      invoke.sender,
      AttachedPaymentExtractor.extractPayments(invoke, V4, blockchain, DAppTarget).explicitGet(),
      ByteStr(invoke.dAppAddressOrAlias.bytes),
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
        V4,
        ContractLimits.MaxComplexityByVersion(V4),
        continuationFirstStepMode = true
      )
      .explicitGet()
      ._1
      .asInstanceOf[IncompleteResult]
    (resultExpr, unusedComplexity)
  }

  property("continuation in progress result after invoke") {
    val invoke                         = invokeScriptGen(paymentListGen).sample.get.copy(funcCallOpt = Some(FUNCTION_CALL(User("multiStepExpr"), Nil)))
    val stepFee                        = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit
    val blockchain                     = blockchainMock(invoke, ("multiStepExpr", 1234L), None)
    val (resultExpr, unusedComplexity) = evaluateInvokeFirstStep(invoke, blockchain)

    val sigVerifyComplexity = 200
    unusedComplexity should be < sigVerifyComplexity
    val spentComplexity = ContractLimits.MaxComplexityByVersion(V4) - unusedComplexity

    InvokeScriptTransactionDiff(blockchain, invoke.timestamp, false)(invoke).resultE shouldBe Right(
      Diff.empty.copy(
        transactions = Map(invoke.id.value()            -> NewTransactionInfo(invoke, Set(), ScriptExecutionInProgress)),
        portfolios = Map(invoke.sender.toAddress        -> Portfolio.waves(-stepFee)),
        continuationStates = Map((invoke.id.value(), 0) -> ContinuationState.InProgress(resultExpr, unusedComplexity)),
        scriptsRun = 1,
        scriptsComplexity = spentComplexity
      )
    )
  }

  property("continuation in progress result after continuation") {
    val expr                             = dApp.expr.callableFuncs.find(_.u.name == "multiStepExpr").get.u.body
    val step                             = Random.nextInt(Int.MaxValue)
    val invoke                           = invokeScriptGen(paymentListGen).sample.get.copy(funcCallOpt = Some(FUNCTION_CALL(User("multiStepExpr"), Nil)))
    val continuation                     = ContinuationTransaction(invoke.id.value(), invoke.timestamp, step, fee = 0L, invoke.feeAssetId)
    val stepFee                          = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit
    val startUnusedComplexity            = Random.nextInt(2000)
    val (result, resultUnusedComplexity) = evaluateContinuationStep(expr, startUnusedComplexity)
    val blockchain                       = blockchainMock(invoke, ("multiStepExpr", 1234L), Some((step, expr, startUnusedComplexity)))

    val sigVerifyComplexity = 200
    resultUnusedComplexity should be < sigVerifyComplexity
    val spentComplexity = ContractLimits.MaxComplexityByVersion(V4) + startUnusedComplexity - resultUnusedComplexity

    ContinuationTransactionDiff(blockchain, continuation.timestamp, false)(continuation).resultE shouldBe Right(
      Diff.empty.copy(
        portfolios = Map(invoke.sender.toAddress -> Portfolio.waves(-stepFee)),
        replacingTransactions = Seq(NewTransactionInfo(continuation.copy(fee = stepFee), Set(), Succeeded)),
        continuationStates = Map((invoke.id.value(), continuation.step + 1) -> ContinuationState.InProgress(result, resultUnusedComplexity)),
        scriptsRun = 1,
        scriptsComplexity = spentComplexity
      )
    )
  }

  property("continuation finish result") {
    val step         = Random.nextInt(Int.MaxValue)
    val invoke       = invokeScriptGen(paymentListGen).sample.get.copy(funcCallOpt = Some(FUNCTION_CALL(User("oneStepExpr"), Nil)))
    val continuation = ContinuationTransaction(invoke.id.value(), invoke.timestamp, step, fee = 0L, invoke.feeAssetId)
    val stepFee      = FeeConstants(InvokeScriptTransaction.typeId) * FeeUnit
    val dAppAddress  = invoke.dAppAddressOrAlias.asInstanceOf[Address]
    val expr         = dApp.expr.callableFuncs.find(_.u.name == "oneStepExpr").get.u.body

    val actualComplexity    = 2015
    val estimatedComplexity = actualComplexity + Random.nextInt(2000)
    val blockchain          = blockchainMock(invoke, ("oneStepExpr", estimatedComplexity), Some((step, expr, 0)))

    ContinuationTransactionDiff(blockchain, continuation.timestamp, false)(continuation).resultE shouldBe Right(
      Diff.empty.copy(
        portfolios = Map(invoke.sender.toAddress -> Portfolio.waves(-stepFee)),
        accountData = Map(dAppAddress            -> AccountDataInfo(Map("isAllowed" -> BooleanDataEntry("isAllowed", true)))),
        scriptsRun = 1,
        scriptResults = Map(invoke.id.value() -> InvokeScriptResult(Seq(BooleanDataEntry("isAllowed", true)))),
        scriptsComplexity = actualComplexity,
        continuationStates = Map((invoke.id.value(), continuation.step + 1) -> ContinuationState.Finished),
        replacingTransactions = Seq(
          NewTransactionInfo(continuation.copy(fee = stepFee), Set(), Succeeded),
          NewTransactionInfo(invoke, Set(invoke.sender.toAddress, dAppAddress), Succeeded)
        )
      )
    )
  }
}
