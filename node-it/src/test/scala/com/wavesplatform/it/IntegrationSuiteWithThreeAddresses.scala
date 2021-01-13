package com.wavesplatform.it

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.ScorexLogging
import org.scalatest._

trait IntegrationSuiteWithThreeAddresses extends BaseSuiteLike with IntegrationTestsScheme with ScorexLogging {
  this: TestSuite =>

  protected lazy val firstKeyPair: KeyPair = miner.createKeyPair()
  protected lazy val firstAddress: String  = firstKeyPair.toAddress.toString

  protected lazy val secondKeyPair: KeyPair = miner.createKeyPair()
  protected lazy val secondAddress: String  = secondKeyPair.toAddress.toString

  protected lazy val thirdKeyPair: KeyPair = miner.createKeyPair()
  protected lazy val thirdAddress: String  = thirdKeyPair.toAddress.toString

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()

    val defaultBalance: Long = 100.waves

    miner.massTransfer(
      miner.keyPair,
      List(firstAddress, secondAddress, thirdAddress).map(a => MassTransferTransaction.Transfer(a, defaultBalance)),
      0.003.waves,
      waitForTx = true,
      version = 1
    )
  }

  def setContract(contractText: Option[String], acc: KeyPair): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
    }
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(1.toByte, acc, script, 0.014.waves, System.currentTimeMillis())
      .explicitGet()
    miner
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }

  def setContracts(contracts: (Option[String], KeyPair)*): Unit = {
    contracts
      .map {
        case (src, acc) => setContract(src, acc)
      }
      .foreach(id => miner.waitForTransaction(id))
    nodes.waitForHeightArise()
  }
}
