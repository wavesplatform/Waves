package com.wavesplatform.it

import com.wavesplatform.account.{KeyPair, SeedKeyPair}
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.*
import org.scalatest.*

trait IntegrationSuiteWithThreeAddresses extends BaseSuite {
  this: TestSuite & Nodes =>

  protected lazy val firstKeyPair: SeedKeyPair = SeedKeyPair("firstKeyPair".getBytes())
  protected lazy val firstAddress: String      = firstKeyPair.toAddress.toString

  protected lazy val secondKeyPair: KeyPair = SeedKeyPair("secondKeyPair".getBytes())
  protected lazy val secondAddress: String  = secondKeyPair.toAddress.toString

  protected lazy val thirdKeyPair: KeyPair = SeedKeyPair("thirdKeyPair".getBytes())
  protected lazy val thirdAddress: String  = thirdKeyPair.toAddress.toString

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()

    withClue("beforeAll") {
      nodes.waitForHeightAriseAndTxPresent(
        sender.massTransfer(sender.keyPair, List(firstAddress, secondAddress, thirdAddress).map(MassTransferTransaction.Transfer(_, 100.waves)), 0.003.waves, version = 1.toByte).id
      )
    }
  }

  def setContract(contractText: Option[String], acc: KeyPair): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
    }
    val setScriptTransaction = script.fold(TxHelpers.removeScript(acc, 0.014.waves)) { s =>
      TxHelpers.setScript(acc, s, 0.014.waves)
    }

    sender
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }

  def setContracts(contracts: (Option[String], KeyPair)*): Unit = {
    contracts
      .map { case (src, acc) =>
        setContract(src, acc)
      }
      .foreach(id => sender.waitForTransaction(id))
    nodes.waitForHeightArise()
  }
}
