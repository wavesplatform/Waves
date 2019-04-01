package com.wavesplatform.it

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, Suite}

trait MatcherNode extends BeforeAndAfterAll with Nodes with ScorexLogging {
  this: Suite =>

  def matcherNode: Node = nodes.head
  def aliceNode: Node   = nodes(1)
  def bobNode: Node     = nodes(2)

  protected lazy val matcherAddress: String = matcherNode.createAddress()
  protected lazy val aliceAddress: String   = aliceNode.createAddress()
  protected lazy val bobAddress: String     = bobNode.createAddress()

  protected lazy val matcherAcc = KeyPair.fromSeed(matcherNode.seed(matcherAddress)).right.get
  protected lazy val aliceAcc   = KeyPair.fromSeed(aliceNode.seed(aliceAddress)).right.get
  protected lazy val bobAcc     = KeyPair.fromSeed(bobNode.seed(bobAddress)).right.get

  private val addresses = Seq(matcherAddress, aliceAddress, bobAddress)

  //really before all tests, because FreeSpec issue with "-" and "in"
  initialBalances()

  def initialBalances(): Unit = {
    List(matcherAddress, aliceAddress, bobAddress)
      .map { i =>
        aliceNode.transfer(aliceNode.address, i, 10000.waves, 0.001.waves).id
      }
      .foreach(nodes.waitForTransaction)
  }

  def initialScripts(): Unit = {
    for (i <- List(matcherNode, aliceNode, bobNode).indices) {
      val script = ScriptCompiler("true", isAssetScript = false).explicitGet()._1
      val pk     = KeyPair(Base58.decode(nodes(i).seed(addresses(i))))
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(pk, Some(script), 0.01.waves, System.currentTimeMillis())
        .explicitGet()

      matcherNode
        .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
    }
  }

  def setContract(contractText: Option[String], acc: KeyPair): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
    }
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(acc, script, 0.014.waves, System.currentTimeMillis())
      .explicitGet()

    matcherNode
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }
}
