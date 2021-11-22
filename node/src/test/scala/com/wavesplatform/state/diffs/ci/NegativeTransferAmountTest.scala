package com.wavesplatform.state.diffs.ci

import com.wavesplatform.TransactionGenBase
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxVersion}

class NegativeTransferAmountTest extends PropSpec with WithDomain with TransactionGenBase {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def dApp1Script(asset: Asset, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    [
         |      ScriptTransfer(i.caller, -1, base58'$asset')
         |    ]
         | }
       """.stripMargin
    )

  private def scenario(bigComplexityDApp: Boolean) =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
      itx      = IssueTransaction.selfSigned(1.toByte, dApp1, "name", "", 100, 0, true, None, fee, ts).explicitGet()
      asset    = IssuedAsset(itx.id.value())
      ssTx1    = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script(asset, bigComplexityDApp)), fee, ts).explicitGet()
      invokeTx = () => Signed.invokeScript(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts)
    } yield (Seq(gTx1, gTx2, itx, ssTx1), invokeTx, dApp1.toAddress, asset)

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(syncDAppCheckTransfersHeight = 3)

  property("negative transfer amount") {
    for (bigComplexity <- Seq(false, true)) {
      val (preparingTxs, invoke, dApp, asset) = scenario(bigComplexity).sample.get
      withDomain(domainSettingsWithFS(settings)) { d =>
        d.appendBlock(preparingTxs: _*)

        val invoke1 = invoke()
        d.appendBlock(invoke1)
        d.blockchain.bestLiquidDiff.get.errorMessage(invoke1.txId).get.text shouldBe "Negative amount"

        val invoke2 = invoke()
        (the[Exception] thrownBy d.appendBlock(invoke2)).getMessage should include("Negative transfer amount = -1")
      }
    }
  }
}
