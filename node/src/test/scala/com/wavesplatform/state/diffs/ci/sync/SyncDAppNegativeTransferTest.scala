package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.TransactionGenBase
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures._
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.{Asset, GenesisTransaction, TxVersion}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed

class SyncDAppNegativeTransferTest extends PropSpec with WithDomain with TransactionGenBase {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def sigVerify(c: Boolean) =
    s""" strict c = ${if (c) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "true"} """

  private def dApp1Script(dApp2: Address, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |    ${sigVerify(bigComplexity)}
         |    strict r = Address(base58'$dApp2').invoke("default", [], [])
         |    []
         | }
       """.stripMargin
    )

  private def dApp2Script(asset: Asset, bigComplexity: Boolean): Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   ${sigVerify(bigComplexity)}
         |   [
         |     ScriptTransfer(i.caller, -1, base58'$asset')
         |   ]
         | }
       """.stripMargin
    )

  private def scenario(bigComplexityDApp1: Boolean, bigComplexityDApp2: Boolean) =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      dApp2   <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3     = GenesisTransaction.create(dApp2.toAddress, fee * 3, ts).explicitGet()
      itx      = IssueTransaction.selfSigned(1.toByte, dApp2, "name", "", 100, 0, true, None, fee, ts).explicitGet()
      asset    = IssuedAsset(itx.id.value())
      ssTx1    = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script(dApp2.toAddress, bigComplexityDApp1)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, dApp2, Some(dApp2Script(asset, bigComplexityDApp2)), fee, ts).explicitGet()
      invokeTx = () => Signed.invokeScript(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts)
    } yield (Seq(gTx1, gTx2, gTx3, itx, ssTx1, ssTx2), invokeTx, dApp2.toAddress, asset)

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)

  property("negative transfer amount") {
    for {
      bigComplexityDApp1 <- Seq(false, true)
      bigComplexityDApp2 <- Seq(false, true)
    } {
      val (preparingTxs, invoke, _, _) = scenario(bigComplexityDApp1, bigComplexityDApp2).sample.get

      withDomain(domainSettingsWithFS(settings)) { d =>
        d.appendBlock(preparingTxs: _*)

        val invoke1 = invoke()
        if (!bigComplexityDApp1 && !bigComplexityDApp2) {
          d.appendAndCatchError(invoke1).toString should include("Negative transfer amount")
        } else {
          d.appendAndAssertFailed(invoke1)
        }
      }
    }
  }
}
