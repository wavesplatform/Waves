package com.wavesplatform.it

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
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

  protected lazy val firstAddress: String  = sender.createAddress()
  protected lazy val secondAddress: String = sender.createAddress()
  protected lazy val thirdAddress: String  = sender.createAddress()

  def pkByAddress(address: String): KeyPair =
    KeyPair(Base58.decode(sender.seed(address)))

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()

    val defaultBalance: Long = 100.waves

    def dumpBalances(node: Node, accounts: Seq[String], label: String): Unit = {
      accounts.foreach(acc => {
        val (balance, eff) = miner.accountBalances(acc)

        val formatted = s"$acc: balance = $balance, effective = $eff"
        log.debug(s"$label account balance:\n$formatted")
      })
    }

    def waitForTxsToReachAllNodes(txIds: Seq[String]) = {
      val txNodePairs = for {
        txId <- txIds
        node <- nodes
      } yield (node, txId)

      txNodePairs.foreach({ case (node, tx) => node.waitForTransaction(tx) })
    }

    def makeTransfers(accounts: Seq[String]): Seq[String] = accounts.map { acc =>
      sender.transfer(sender.address, acc, defaultBalance, sender.fee(TransferTransactionV1.typeId)).id

    }

    def correctStartBalancesFuture(): Unit = {
      nodes.waitForHeight(2)
      val accounts = Seq(firstAddress, secondAddress, thirdAddress)

      dumpBalances(sender, accounts, "initial")
      val txs = makeTransfers(accounts)

      val height = nodes.map(_.height).max

      withClue(s"waitForHeight(${height + 2})") {
        nodes.waitForHeight(height + 2)
      }

      withClue("waitForTxsToReachAllNodes") {
        waitForTxsToReachAllNodes(txs)
      }

      dumpBalances(sender, accounts, "after transfer")
      accounts.foreach(miner.assertBalances(_, defaultBalance, defaultBalance))
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
      .selfSigned(acc, script, 0.014.waves, System.currentTimeMillis())
      .right
      .get
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
