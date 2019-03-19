package com.wavesplatform.it

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, Suite}

trait MatcherNode extends BeforeAndAfterAll with Nodes with ScorexLogging {
  this: Suite =>

  def matcherNode: Node = nodes.head
  def aliceNode: Node   = nodes.head //(1)
  def bobNode: Node     = nodes.head //(2)

  protected lazy val matcherAddress: String = ??? // matcherNode.createAddress()
  protected lazy val aliceAddress: String   = ??? // aliceNode.createAddress()
  protected lazy val bobAddress: String     = ??? /// bobNode.createAddress()

  protected lazy val matcherAcc: PrivateKeyAccount = ??? // PrivateKeyAccount("1".getBytes) // .fromSeed(matcherNode.seed(matcherAddress)).right.get
  protected lazy val aliceAcc: PrivateKeyAccount   = ??? // PrivateKeyAccount("2".getBytes) //.fromSeed(aliceNode.seed(aliceAddress)).right.get
  protected lazy val bobAcc: PrivateKeyAccount     = ??? // PrivateKeyAccount("3".getBytes) //.fromSeed(bobNode.seed(bobAddress)).right.get

  private lazy val addresses = Seq(matcherAddress, aliceAddress, bobAddress)

  //really before all tests, because FreeSpec issue with "-" and "in"
//  initialBalances()

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
      val pk     = PrivateKeyAccount.fromSeed(nodes(i).seed(addresses(i))).right.get
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(pk, Some(script), 0.01.waves, System.currentTimeMillis())
        .explicitGet()

      matcherNode
        .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
    }
  }

  def setContract(contractText: Option[String], acc: PrivateKeyAccount): String = {
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
