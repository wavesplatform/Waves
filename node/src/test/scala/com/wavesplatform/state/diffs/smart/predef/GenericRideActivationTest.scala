package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3, V4}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.utils.Signed
import org.scalatest.EitherValues

class GenericRideActivationTest extends PropSpec with WithDomain with EitherValues {

  import DomainPresets.*

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private def dApp(version: StdLibVersion) = TestCompiler(version).compileContract(
    s"""
       | @Callable(i)
       | func default() = ${if (version == V3) "WriteSet([])" else "[]"}
     """.stripMargin
  )

  private def verifier(version: StdLibVersion) = TestCompiler(version).compileExpression("true")

  private def scenario(version: StdLibVersion) =
    for {
      master  <- accountGen
      invoker <- accountGen
      fee     <- ciFee(sc = 1)
      gTx1   = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2   = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      ssTx   = SetScriptTransaction.selfSigned(1.toByte, master, Some(dApp(version)), 0.01.waves, ts).explicitGet()
      ssTx2  = SetScriptTransaction.selfSigned(1.toByte, invoker, Some(verifier(version)), 0.01.waves, ts).explicitGet()
      invoke = Signed.invokeScript(1.toByte, invoker, master.toAddress, None, Nil, fee, Waves, ts)
    } yield (Seq(gTx1, gTx2), Seq(ssTx, ssTx2), invoke)

  property("RIDE versions activation") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .zip(DirectiveDictionary[StdLibVersion].all.filter(_ >= V4).map(Some(_)).toList :+ None) // (V3, V4), (V4, V5), ..., (Vn, None)
      .foreach { case (currentVersion, nextVersion) =>
        val (genesisTxs, setScriptTxs, invoke) = scenario(currentVersion).sample.get
        withDomain(settingsForRide(currentVersion)) { d =>
          d.appendBlock(genesisTxs*)
          d.appendBlock(setScriptTxs*)
          d.appendBlock(invoke)
          d.blockchain.transactionSucceeded(invoke.id.value()) shouldBe true
          nextVersion.foreach { v =>
            val (_, setScriptTxs, _) = scenario(v).sample.get
            (the[RuntimeException] thrownBy d.appendBlock(setScriptTxs*)).getMessage should include("ActivationError")
          }
        }
      }
  }
}
