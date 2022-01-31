package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxVersion}
import com.wavesplatform.{TestTime, TransactionGenBase}

class SyncDAppNegativePaymentTest extends PropSpec with WithDomain with TransactionGenBase {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

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

  private def scenario(bigComplexityDApp1: Boolean, bigComplexityDApp2: Boolean, customAsset: Boolean) =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      dApp2   <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3     = GenesisTransaction.create(dApp2.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue    = IssueTransaction.selfSigned(1.toByte, dApp2, "Asset", "", 100, 1, true, None, fee, ts).explicitGet()
      asset    = if (customAsset) IssuedAsset(issue.id()) else Waves
      ssTx1    = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script(dApp2.toAddress, bigComplexityDApp1, asset)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp2Script(bigComplexityDApp2)), fee, ts).explicitGet()
      invokeTx = () => InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3, issue, ssTx1, ssTx2), invokeTx, dApp1.toAddress, dApp2.toAddress, asset)

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(forbidSyncDAppNegativePaymentHeight = 3)

  property("negative sync dApp payments amount rejects tx after forbidSyncDAppNegativePaymentHeight") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
      customAsset        <- Seq(false, true)
    } {
      val (preparingTxs, invoke, dApp1, dApp2, asset) = scenario(bigComplexityDApp1, bigComplexityDApp2, customAsset).sample.get
      withDomain(domainSettingsWithFS(settings)) { d =>
        d.appendBlock(preparingTxs: _*)

        val invoke1 = invoke()
        d.appendBlock(invoke1)
        d.blockchain.transactionSucceeded(invoke1.id.value()) shouldBe true

        d.liquidDiff.portfolios(dApp1) shouldBe Portfolio.build(asset, 1)
        d.liquidDiff.portfolios(dApp2) shouldBe Portfolio.build(asset, -1)

        val invoke2 = invoke()
        (the[RuntimeException] thrownBy d.appendBlock(invoke2)).getMessage should include(
          if (customAsset)
            s"DApp $dApp1 invoked DApp $dApp2 with attached token $asset amount = -1"
          else
            s"DApp $dApp1 invoked DApp $dApp2 with attached WAVES amount = -1"
        )
      }
    }
  }
}
