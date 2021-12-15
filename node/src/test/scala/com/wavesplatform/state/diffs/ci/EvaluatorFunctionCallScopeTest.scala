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
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}

class EvaluatorFunctionCallScopeTest extends PropSpec with WithDomain with TransactionGenBase {
  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val dApp1Script: Script =
    TestCompiler(V5).compileContract(
      s"""
         | @Callable(i)
         | func default() = {
         |   let a = 4
         |   func g(b: Int) = a
         |   func f(a: Int) = g(a)
         |   let r = f(1)
         |   [
         |     IntegerEntry("key", r)
         |   ]
         | }
       """.stripMargin
    )

  private val scenario =
    for {
      invoker <- accountGen
      dApp1   <- accountGen
      fee     <- ciFee()
      gTx1     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(dApp1.toAddress, ENOUGH_AMT, ts).explicitGet()
      ssTx1    = SetScriptTransaction.selfSigned(1.toByte, dApp1, Some(dApp1Script), fee, ts).explicitGet()
      invokeTx = () => Signed.invokeScript(TxVersion.V3, invoker, dApp1.toAddress, None, Nil, fee, Waves, ts)
    } yield (Seq(gTx1, gTx2, ssTx1), invokeTx, dApp1.toAddress)

  private val settings =
    TestFunctionalitySettings
      .withFeatures(BlockV5, SynchronousCalls)
      .copy(estimatorSumOverflowFixHeight = 3)

  property("arg of the first function should NOT overlap var accessed from body of the second function AFTER fix") {
    val (preparingTxs, invoke, dApp) = scenario.sample.get
    withDomain(domainSettingsWithFS(settings)) { d =>
      d.appendBlock(preparingTxs: _*)

      d.appendBlock(invoke())
      d.blockchain.accountData(dApp, "key").get.value shouldBe 1

      val transaction = invoke()
      d.appendBlock(transaction)
      d.blockchain.accountData(dApp, "key").get.value shouldBe 4
    }
  }
}
