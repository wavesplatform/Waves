package com.wavesplatform.state.diffs.ci

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InvokeActionsFeeTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory
    with WithDomain
    with EitherValues {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val activationHeight = 3

  private val fsWithV5 = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> activationHeight
    ),
    estimatorPreCheckHeight = Int.MaxValue
  )

  val assetScript: Script = {
    val script = s"""
                    | {-# STDLIB_VERSION 4        #-}
                    | {-# SCRIPT_TYPE ASSET       #-}
                    | {-# CONTENT_TYPE EXPRESSION #-}
                    |
                    | true
                    |
                    """.stripMargin
    ScriptCompiler.compile(script, ScriptEstimatorV3).explicitGet()._1
  }

  private def dApp(assetId: ByteStr): Script = {
    val script =
      s"""
        | {-# STDLIB_VERSION 4       #-}
        | {-# CONTENT_TYPE   DAPP    #-}
        | {-# SCRIPT_TYPE    ACCOUNT #-}
        |
        | @Callable(i)
        | func default() =
        |  [
        |     ScriptTransfer(i.caller, 1, base58'$assetId')
        |  ]
      """.stripMargin

    val expr     = Parser.parseContract(script).get.value
    val contract = compileContractFromExpr(expr, V4)
    ContractScript(V4, contract).explicitGet()
  }

  private val paymentPreconditions: Gen[(List[Transaction], InvokeScriptTransaction, InvokeScriptTransaction)] =
    for {
      master  <- accountGen
      invoker <- accountGen
      fee     <- ciFee()
    } yield {
      for {
        genesis  <- GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts)
        genesis2 <- GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts)
        issue    <- IssueTransaction.selfSigned(2.toByte, master, "Asset", "Description", ENOUGH_AMT, 8, true, Some(assetScript), fee, ts)
        setDApp  <- SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp(issue.id.value())), fee, ts)
        invoke1  <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, Nil, fee, Waves, ts)
        invoke2  <- InvokeScriptTransaction.selfSigned(1.toByte, invoker, master.toAddress, None, Nil, fee, Waves, ts)
      } yield (List(genesis, genesis2, setDApp, issue), invoke1, invoke2)
    }.explicitGet()

  property(s"fee for asset scripts is not required after activation ${BlockchainFeatures.SynchronousCalls}") {
    val (preparingTxs, invoke1, invoke2) = paymentPreconditions.sample.get
    withDomain(domainSettingsWithFS(fsWithV5)) { d =>
      d.appendBlock(preparingTxs: _*)

      d.appendBlock(invoke1)
      d.blockchain.bestLiquidDiff.get.errorMessage(invoke1.id.value()).get.text should include(
        s"Fee in WAVES for InvokeScriptTransaction (${invoke1.fee} in WAVES) with 1 total scripts invoked does not exceed minimal value of 900000 WAVES"
      )

      d.appendBlock()
      d.blockchainUpdater.height shouldBe activationHeight

      d.appendBlock(invoke2)
      d.blockchain.bestLiquidDiff.get.errorMessage(invoke2.id.value()) shouldBe None
    }
  }
}
