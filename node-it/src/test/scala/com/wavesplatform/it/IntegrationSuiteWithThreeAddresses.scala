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
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

trait IntegrationSuiteWithThreeAddresses
    extends BeforeAndAfterAll
    with Matchers
    with ScalaFutures
    with IntegrationPatience
    with RecoverMethods
    with IntegrationTestsScheme
    with Nodes
    with ScorexLogging {
  this: Suite =>

  def miner: Node    = nodes.head
  def notMiner: Node = nodes.last

  protected def sender: Node = miner

  protected lazy val firstKeyPair: KeyPair = sender.createKeyPair()
  protected lazy val firstAddress: String  = firstKeyPair.toAddress.toString

  protected lazy val secondKeyPair: KeyPair = sender.createKeyPair()
  protected lazy val secondAddress: String  = secondKeyPair.toAddress.toString

  protected lazy val thirdKeyPair: KeyPair = sender.createKeyPair()
  protected lazy val thirdAddress: String  = thirdKeyPair.toAddress.toString

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()

    val defaultBalance: Long = 100.waves

    def dumpBalances(accounts: Seq[KeyPair], label: String): Unit =
      accounts.foreach { acc =>
        val (balance, eff) = miner.accountBalances(acc.toAddress.toString)
        log.debug(s"$label $acc balance: balance = $balance, effective = $eff")
      }

    def makeTransfers(accounts: Seq[KeyPair]): Seq[String] = accounts.map { acc =>
      sender.transfer(sender.keyPair, acc.toAddress.toString, defaultBalance, sender.fee(TransferTransaction.typeId)).id
    }

    def correctStartBalancesFuture(): Unit = {
      nodes.waitForHeight(2)
      val accounts = Seq(firstKeyPair, secondKeyPair, thirdKeyPair)

      dumpBalances(accounts, "initial")
      val txs = makeTransfers(accounts)

      val targetHeight = nodes.map(_.height).max + 1
      withClue(s"waitForHeight($targetHeight)") {
        nodes.waitForHeight(targetHeight)
      }

      withClue("waitForTxsToReachAllNodes") {
        txs.foreach(nodes.waitForTransaction)
      }

      dumpBalances(accounts, "after transfer")
      accounts.foreach(a => miner.assertBalances(a.toAddress.toString, defaultBalance, defaultBalance))
    }

    withClue("beforeAll") {
      correctStartBalancesFuture()
    }
  }

  def setContract(contractText: Option[String], acc: KeyPair): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
    }
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(1.toByte, acc, script, 0.014.waves, System.currentTimeMillis())
      .explicitGet()
    sender
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }

  def setContracts(contracts: (Option[String], KeyPair)*): Unit = {
    contracts
      .map {
        case (src, acc) => setContract(src, acc)
      }
      .foreach(id => sender.waitForTransaction(id))
    nodes.waitForHeightArise()
  }
}
