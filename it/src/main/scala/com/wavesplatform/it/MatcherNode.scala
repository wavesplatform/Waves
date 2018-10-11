package com.wavesplatform.it.transactions

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.it.Nodes
import com.wavesplatform.it.util._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api.libs.json.JsNumber

trait MatcherNode extends BeforeAndAfterAll with Nodes with ScorexLogging {
  this: Suite =>

  def matcherNode = nodes.head
  def aliceNode   = nodes(1)
  def bobNode     = nodes(2)

  protected lazy val matcherAddress: String = matcherNode.createAddress()
  protected lazy val aliceAddress: String   = aliceNode.createAddress()
  protected lazy val bobAddress: String     = bobNode.createAddress()

  protected lazy val matcherAcc = PrivateKeyAccount.fromSeed(matcherNode.seed(matcherAddress)).right.get
  protected lazy val aliceAcc   = PrivateKeyAccount.fromSeed(aliceNode.seed(aliceAddress)).right.get
  protected lazy val bobAcc     = PrivateKeyAccount.fromSeed(bobNode.seed(bobAddress)).right.get

  private val addresses = Seq(matcherAddress, aliceAddress, bobAddress)

  //really before all tests, because FreeSpec issue with "-" and "in"
  initialBalances()

  def initialBalances(): Unit = {
    for (i <- List(matcherNode, aliceNode, bobNode).indices) {

      val tx = nodes(i).transfer(nodes(i).address, addresses(i), 10000.waves, 0.001.waves).id
      nodes.waitForHeightAriseAndTxPresent(tx)
    }
  }

  def initialScripts(): Unit = {
    for (i <- List(matcherNode, aliceNode, bobNode).indices) {

      val scriptText = s"""true""".stripMargin

      val script = ScriptCompiler(scriptText).explicitGet()._1
      val pk     = PrivateKeyAccount.fromSeed(nodes(i).seed(addresses(i))).right.get
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, pk, Some(script), 1.waves, System.currentTimeMillis())
        .right
        .get

      val setScriptId = matcherNode
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
        .id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)
    }
  }
}
