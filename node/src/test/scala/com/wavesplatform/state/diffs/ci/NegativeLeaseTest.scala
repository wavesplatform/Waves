package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers

class NegativeLeaseTest extends PropSpec with WithDomain {

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def dAppScript(bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   [
         |     Lease(i.caller, -1, 0)
         |   ]
         | }
       """.stripMargin
    )

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(syncDAppCheckTransfersHeight = 3)

  property("negative lease amount") {
    for(bigComplexity <- Seq(false, true)) {
      val invoker = TxHelpers.signer(0)
      val dApp = TxHelpers.signer(1)

      val genesis = Seq(
        TxHelpers.genesis(invoker.toAddress),
        TxHelpers.genesis(dApp.toAddress)
      )
      val issue = TxHelpers.issue(dApp, 100)
      val asset = IssuedAsset(issue.id.value())
      val setScript = TxHelpers.setScript(dApp, dAppScript(bigComplexity))

      val preparingTxs = genesis :+ issue :+ setScript

      val invoke1 = TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)
      val invoke2 = TxHelpers.invoke(dApp.toAddress, func = None, invoker = invoker)

      withDomain(domainSettingsWithFS(settings)) { d =>
        d.appendBlock(preparingTxs: _*)

        d.appendBlock(invoke1)
        d.blockchain.bestLiquidDiff.get.errorMessage(invoke1.txId).get.text shouldBe "NonPositiveAmount(-1,waves)"

        (the[Exception] thrownBy d.appendBlock(invoke2)).getMessage should include ("Negative lease amount = -1")
      }
    }
  }
}
