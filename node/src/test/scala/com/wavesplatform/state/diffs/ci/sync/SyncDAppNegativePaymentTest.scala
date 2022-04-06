package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, TxHelpers}

class SyncDAppNegativePaymentTest extends PropSpec with WithDomain {

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def dApp1Script(dApp2: Address, bigComplexity: Boolean, asset: Asset): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   let asset = ${asset.fold("unit")(a => s"base58'$a'")}
         |   strict r = Address(base58'$dApp2').invoke("default", [], [AttachedPayment(asset, -1)])
         |   []
         | }
       """.stripMargin
    )

  private def dApp2Script(bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   []
         | }
       """.stripMargin
    )

  private def scenario(bigComplexityDApp1: Boolean, bigComplexityDApp2: Boolean, customAsset: Boolean) = {
    val invoker = TxHelpers.signer(0)
    val dApp1   = TxHelpers.signer(1)
    val dApp2   = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

    val issue = TxHelpers.issue(dApp2, 100)
    val asset = if (customAsset) IssuedAsset(issue.id()) else Waves
    val setScript = Seq(
      TxHelpers.setScript(dApp1, dApp1Script(dApp2.toAddress, bigComplexityDApp1, asset)),
      TxHelpers.setScript(dApp2, dApp2Script(bigComplexityDApp2))
    )

    val invoke = () => TxHelpers.invoke(dApp1.toAddress, invoker = invoker)

    (balances, issue +: setScript, invoke, dApp1.toAddress, dApp2.toAddress, asset)
  }

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(enforceTransferValidationAfter = 4)

  property("negative sync dApp payments amount rejects tx after enforceTransferValidationAfter") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
      customAsset        <- Seq(false, true)
    } {
      val (balances, preparingTxs, invoke, dApp1, dApp2, asset) = scenario(bigComplexityDApp1, bigComplexityDApp2, customAsset)
      withDomain(domainSettingsWithFS(settings), balances) { d =>
        d.appendBlock(preparingTxs*)

        val invoke1 = invoke()
        d.appendBlock(invoke1)
        d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true

        d.liquidDiff.portfolios(dApp1) shouldBe Portfolio.build(asset, 1)
        d.liquidDiff.portfolios(dApp2) shouldBe Portfolio.build(asset, -1)

        d.appendBlock()
        val invoke2 = invoke()
        d.createDiffE(invoke2) should produceRejectOrFailedDiff {
          if (customAsset)
              s"DApp $dApp1 invoked DApp $dApp2 with attached token $asset amount = -1"
          else
            s"DApp $dApp1 invoked DApp $dApp2 with attached WAVES amount = -1"
        }
      }
    }
  }
}
