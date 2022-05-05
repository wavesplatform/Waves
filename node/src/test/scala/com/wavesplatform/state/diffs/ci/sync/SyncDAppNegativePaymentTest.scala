package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.produceRejectOrFailedDiff
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{Asset, TxHelpers}

class SyncDAppNegativePaymentTest extends PropSpec with WithDomain {

  property("negative sync dApp payments amount rejects tx after enforceTransferValidationAfter") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
      customAsset        <- Seq(false, true)
    } {
      val (balances, preparingTxs, invoke, dApp1, dApp2, asset) = scenario(bigComplexityDApp1, bigComplexityDApp2, customAsset, -1)
      withDomain(DomainPresets.RideV5.configure(_.copy(enforceTransferValidationAfter = 4)), balances) { d =>
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

  property("negative sync dApp payments are forbidden before and after RideV6 activation") {
    val (balances, preparingTxs, invoke, dApp1, dApp2, _) =
      scenario(bigComplexityDApp1 = false, bigComplexityDApp2 = false, customAsset = false, -1)
    withDomain(DomainPresets.RideV5.configure(_.copy(enforceTransferValidationAfter = 0)).setFeaturesHeight(RideV6 -> 4), balances) { d =>
      val errMsg = s"DApp $dApp1 invoked DApp $dApp2 with attached WAVES amount = -1"

      d.appendBlock(preparingTxs*)

      val invoke1 = invoke()
      d.appendBlockE(invoke1) should produceRejectOrFailedDiff(errMsg)

      d.appendBlock()
      val invoke2 = invoke()
      d.appendBlockE(invoke2) should produceRejectOrFailedDiff(errMsg)
    }
  }

  property("zero sync dApp payments are allowed before and forbidden after RideV6 activation") {
    val (balances, preparingTxs, invoke, dApp1, dApp2, _) =
      scenario(bigComplexityDApp1 = false, bigComplexityDApp2 = false, customAsset = false, 0)
    withDomain(DomainPresets.RideV5.configure(_.copy(enforceTransferValidationAfter = 0)).setFeaturesHeight(RideV6 -> 4), balances) { d =>
      d.appendBlock(preparingTxs*)

      val invoke1 = invoke()
      d.appendAndAssertSucceed(invoke1)

      val invoke2 = invoke()
      d.appendBlockE(invoke2) should produceRejectOrFailedDiff(s"DApp $dApp1 invoked DApp $dApp2 with attached WAVES amount = 0")
    }
  }

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def invokeDAppScript(dApp2: Address, bigComplexity: Boolean, asset: Asset, amount: Long): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   let asset = ${asset.fold("unit")(a => s"base58'$a'")}
         |   strict r = Address(base58'$dApp2').invoke("default", [], [AttachedPayment(asset, $amount)])
         |   []
         | }
       """.stripMargin
    )

  private def simpleDAppScript(bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   []
         | }
       """.stripMargin
    )

  private def scenario(bigComplexityDApp1: Boolean, bigComplexityDApp2: Boolean, customAsset: Boolean, amount: Long) = {
    val invoker = TxHelpers.signer(0)
    val dApp1   = TxHelpers.signer(1)
    val dApp2   = TxHelpers.signer(2)

    val balances = AddrWithBalance.enoughBalances(invoker, dApp1, dApp2)

    val issue = TxHelpers.issue(dApp2, 100)
    val asset = if (customAsset) IssuedAsset(issue.id()) else Waves
    val setScript = Seq(
      TxHelpers.setScript(dApp1, invokeDAppScript(dApp2.toAddress, bigComplexityDApp1, asset, amount)),
      TxHelpers.setScript(dApp2, simpleDAppScript(bigComplexityDApp2))
    )

    val invoke = () => TxHelpers.invoke(dApp1.toAddress, invoker = invoker)

    (balances, issue +: setScript, invoke, dApp1.toAddress, dApp2.toAddress, asset)
  }
}
