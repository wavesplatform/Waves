package com.wavesplatform.state.diffs.smart.scenarios

import cats.syntax.semigroup.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.SnapshotOps
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Asset as AssetType, *}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, TestCompiler}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.*
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.*
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.test.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.*

class BalancesV4Test extends PropSpec with WithState {

  val MinFee: Long            = Constants.UnitsInWave / 1000L
  val DataTxFee: Long         = 15000000L
  val InvokeScriptTxFee: Long = 15000000L
  val MassTransferTxFee: Long = 15000000L
  val SetScriptFee: Long      = Constants.UnitsInWave / 1000L
  val SetAssetScriptFee: Long = Constants.UnitsInWave

  val rideV4Activated: FunctionalitySettings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures =
    Map(
      BlockchainFeatures.Ride4DApps.id    -> 0,
      BlockchainFeatures.SmartAccounts.id -> 0,
      BlockchainFeatures.BlockV5.id       -> 0
    )
  )

  private val preconditionsAndTransfer = {
    val master = TxHelpers.signer(0)
    val acc1   = TxHelpers.signer(1)
    val dapp   = TxHelpers.signer(2)

    val genesis = Seq(
      TxHelpers.genesis(master.toAddress),
      TxHelpers.genesis(acc1.toAddress, 25 * Constants.UnitsInWave + 3 * MinFee),
      TxHelpers.genesis(dapp.toAddress, 10 * Constants.UnitsInWave + SetScriptFee + 2 * InvokeScriptTxFee + 1 * Constants.UnitsInWave)
    )
    val alias       = "alias"
    val createAlias = TxHelpers.createAlias(alias, acc1, MinFee)
    val setScript   = TxHelpers.setScript(dapp, script(alias), SetScriptFee)
    val invoke      = TxHelpers.invoke(dapp.toAddress, func = Some("bar"), invoker = master, fee = InvokeScriptTxFee)
    val lease1      = TxHelpers.lease(acc1, dapp.toAddress, 10 * Constants.UnitsInWave, MinFee)
    val lease2      = TxHelpers.lease(acc1, dapp.toAddress, 10 * Constants.UnitsInWave, MinFee)
    val leaseD      = TxHelpers.lease(dapp, acc1.toAddress, 1 * Constants.UnitsInWave, MinFee)
    val cancel1     = TxHelpers.leaseCancel(lease1.id(), acc1, MinFee)
    val transfer    = TxHelpers.transfer(dapp, acc1.toAddress, 1 * Constants.UnitsInWave + MinFee, fee = InvokeScriptTxFee)

    ((genesis :+ createAlias) ++ Seq(setScript, lease1, lease2), Seq(cancel1, leaseD, transfer), acc1, dapp, invoke)
  }

  def script(a: String): Script = {
    val script =
      s"""
         | {-#STDLIB_VERSION 4 #-}
         | {-#SCRIPT_TYPE ACCOUNT #-}
         | {-#CONTENT_TYPE DAPP #-}
         |
         | @Callable(i)
         | func bar() = {
         |  let balance = wavesBalance(Alias("$a"))
         |   [
         |     IntegerEntry("available", balance.available),
         |     IntegerEntry("regular", balance.regular),
         |     IntegerEntry("generating", balance.generating),
         |     IntegerEntry("effective", balance.effective)
         |   ]
         | }
      """.stripMargin
    TestCompiler(V4).compileContract(script)
  }

  property("Waves balance details") {
    val (genesis, b, acc1, dapp, ci) = preconditionsAndTransfer
    assertDiffAndState(
      Seq(TestBlock.create(genesis)) ++
        (0 to 1000).map(_ => TestBlock.create(Seq())) ++
        Seq(TestBlock.create(b)),
      TestBlock.create(Seq(ci)),
      rideV4Activated
    ) { case (d, s) =>
      val apiBalance =
        com.wavesplatform.api.common
          .CommonAccountsApi(() => SnapshotBlockchain(s, SnapshotOps.fromDiff(d, s).explicitGet()), rdb, s)
          .balanceDetails(acc1.toAddress)
          .explicitGet()
      val data = d.accountData(dapp.toAddress)
      data("available") shouldBe IntegerDataEntry("available", apiBalance.available)
      apiBalance.available shouldBe 16 * Constants.UnitsInWave
      data("regular") shouldBe IntegerDataEntry("regular", apiBalance.regular)
      apiBalance.regular shouldBe 26 * Constants.UnitsInWave
      data("generating") shouldBe IntegerDataEntry("generating", apiBalance.generating)
      apiBalance.generating shouldBe 5 * Constants.UnitsInWave
      data("effective") shouldBe IntegerDataEntry("effective", apiBalance.effective)
      apiBalance.effective shouldBe 17 * Constants.UnitsInWave

    }
  }

  property("Asset balance change while processing script result") {
    val a = 10000000000L
    def assetScript(acc: ByteStr): Script = {
      val ctx = {
        val directives = DirectiveSet(V4, AssetType, Expression).explicitGet()
        PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
          WavesContext.build(Global, directives, fixBigScriptField = true)
      }

      val script =
        s"""
           | {-# STDLIB_VERSION 4 #-}
           | {-# CONTENT_TYPE EXPRESSION #-}
           | {-# SCRIPT_TYPE ASSET #-}
           |
           | assetBalance(Address(base58'$acc'), this.id) == $a && assetBalance(Alias("alias"), this.id) == $a
        """.stripMargin
      val parsedScript = Parser.parseExpr(script).get.value
      ExprScript(V4, ExpressionCompiler(ctx.compilerContext, parsedScript).explicitGet()._1)
        .explicitGet()
    }

    def dappScript(acc: ByteStr, asset: ByteStr): Script = {
      val script =
        s"""
           | {-#STDLIB_VERSION 4 #-}
           | {-#SCRIPT_TYPE ACCOUNT #-}
           | {-#CONTENT_TYPE DAPP #-}
           |
           | @Callable(i)
           | func bar() = {
           |   [
           |    ScriptTransfer(Address(base58'$acc'), 1, base58'$asset'),
           |    Reissue(base58'$asset', 2, false)
           |   ]
           | }
        """.stripMargin
      TestCompiler(V4).compileContract(script)
    }

    val acc1 = TxHelpers.signer(0)
    val acc2 = TxHelpers.signer(1)

    val genesis = Seq(
      TxHelpers.genesis(acc1.toAddress),
      TxHelpers.genesis(acc2.toAddress)
    )
    val createAlias = TxHelpers.createAlias("alias", acc2, MinFee)
    val issue       = TxHelpers.issue(acc1, 10000000000L, script = Some(assetScript(ByteStr(acc1.toAddress.bytes))), fee = SetAssetScriptFee)
    val setScript   = TxHelpers.setScript(acc1, dappScript(ByteStr(acc2.toAddress.bytes), issue.id()), SetScriptFee)
    val invoke      = TxHelpers.invoke(acc1.toAddress, func = Some("bar"), invoker = acc1, fee = InvokeScriptTxFee)

    assertDiffAndState(Seq(TestBlock.create(genesis :+ createAlias :+ issue :+ setScript)), TestBlock.create(Seq(invoke)), rideV4Activated) {
      case (d, s) =>
        val error = d.scriptResults(invoke.id()).error
        error.get.code shouldBe 3
        error.get.text should include("Transaction is not allowed by script of the asset")
        s.balance(acc1.toAddress, IssuedAsset(issue.id())) shouldBe a
    }
  }

  property("Waves balance change while processing script result") {
    val w = ENOUGH_AMT - SetScriptFee - SetAssetScriptFee
    def assetScript(acc: ByteStr): Script = {
      val ctx = {
        val directives = DirectiveSet(V4, AssetType, Expression).explicitGet()
        PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment] |+|
          CryptoContext.build(Global, V4).withEnvironment[Environment] |+|
          WavesContext.build(Global, directives, fixBigScriptField = true)
      }

      val script =
        s"""
           | {-# STDLIB_VERSION 4 #-}
           | {-# CONTENT_TYPE EXPRESSION #-}
           | {-# SCRIPT_TYPE ASSET #-}
           |
           | wavesBalance(Address(base58'$acc')).regular == $w
        """.stripMargin
      val parsedScript = Parser.parseExpr(script).get.value
      ExprScript(V4, ExpressionCompiler(ctx.compilerContext, parsedScript).explicitGet()._1)
        .explicitGet()
    }

    def dappScript(acc: ByteStr, asset: ByteStr): Script = {
      val script =
        s"""
           | {-#STDLIB_VERSION 4 #-}
           | {-#SCRIPT_TYPE ACCOUNT #-}
           | {-#CONTENT_TYPE DAPP #-}
           |
           | @Callable(i)
           | func bar() = {
           |   [
           |    ScriptTransfer(Address(base58'$acc'), 1, unit),
           |    Reissue(base58'$asset', 1, false)
           |   ]
           | }
        """.stripMargin
      TestCompiler(V4).compileContract(script)
    }

    val acc1 = TxHelpers.signer(0)
    val acc2 = TxHelpers.signer(1)

    val genesis = Seq(
      TxHelpers.genesis(acc1.toAddress),
      TxHelpers.genesis(acc2.toAddress)
    )
    val issue     = TxHelpers.issue(acc1, 10000000000L, script = Some(assetScript(ByteStr(acc1.toAddress.bytes))), fee = SetAssetScriptFee)
    val setScript = TxHelpers.setScript(acc1, dappScript(ByteStr(acc2.toAddress.bytes), issue.id()), SetScriptFee)
    val invoke    = TxHelpers.invoke(acc1.toAddress, func = Some("bar"), invoker = acc2, fee = InvokeScriptTxFee)

    assertDiffAndState(Seq(TestBlock.create(genesis :+ issue :+ setScript)), TestBlock.create(Seq(invoke)), rideV4Activated) { case (d, s) =>
      val error = d.scriptResults(invoke.id()).error
      error.get.code shouldBe 3
      error.get.text should include("Transaction is not allowed by script of the asset")
      s.wavesPortfolio(acc1.toAddress).balance shouldBe w
    }
  }

}
