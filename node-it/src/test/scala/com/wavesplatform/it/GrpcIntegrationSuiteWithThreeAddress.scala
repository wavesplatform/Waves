package com.wavesplatform.it

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions, Recipient}
import com.wavesplatform.test.NumericExt
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.utils.ScorexLogging
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

trait GrpcIntegrationSuiteWithThreeAddress extends BaseSuite with ScalaFutures with IntegrationPatience with RecoverMethods with ScorexLogging {
  this: TestSuite with Nodes =>

  protected lazy val firstAcc: KeyPair  = KeyPair("first_acc".getBytes("UTF-8"))
  protected lazy val secondAcc: KeyPair = KeyPair("second_acc".getBytes("UTF-8"))
  protected lazy val thirdAcc: KeyPair  = KeyPair("third_acc".getBytes("UTF-8"))

  protected lazy val firstAddress: ByteString  = PBRecipients.create(Address.fromPublicKey(firstAcc.publicKey)).getPublicKeyHash
  protected lazy val secondAddress: ByteString = PBRecipients.create(Address.fromPublicKey(secondAcc.publicKey)).getPublicKeyHash
  protected lazy val thirdAddress: ByteString  = PBRecipients.create(Address.fromPublicKey(thirdAcc.publicKey)).getPublicKeyHash

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()

    val defaultBalance: Long = 100.waves

    def dumpBalances(node: Node, accounts: Seq[ByteString], label: String): Unit = {
      accounts.foreach(acc => {
        val balance = node.wavesBalance(acc).available
        val eff     = node.wavesBalance(acc).effective

        val formatted = s"$acc: balance = $balance, effective = $eff"
        log.debug(s"$label account balance:\n$formatted")
      })
    }

    def waitForTxsToReachAllNodes(txIds: Seq[String]): Unit = {
      val txNodePairs = for {
        txId <- txIds
        node <- nodes
      } yield (node, txId)

      txNodePairs.foreach({ case (node, tx) => node.waitForTransaction(tx) })
    }

    def makeTransfers(accounts: Seq[ByteString]): Seq[String] = accounts.map { acc =>
      PBTransactions
        .vanilla(
          sender.broadcastTransfer(sender.keyPair, Recipient().withPublicKeyHash(acc), defaultBalance, sender.fee(TransferTransaction.typeId)),
          unsafe = false
        )
        .explicitGet()
        .id()
        .toString
    }

    def correctStartBalancesFuture(): Unit = {
      nodes.foreach(n => n.waitForHeight(2))
      val accounts = Seq(firstAddress, secondAddress, thirdAddress)

      dumpBalances(sender, accounts, "initial")
      val txs = makeTransfers(accounts)

      val height = nodes.map(_.height).max

      withClue(s"waitForHeight(${height + 2})") {
        nodes.foreach(n => n.waitForHeight(height + 1))
        nodes.foreach(n => n.waitForHeight(height + 2))
      }

      withClue("waitForTxsToReachAllNodes") {
        waitForTxsToReachAllNodes(txs)
      }

      dumpBalances(sender, accounts, "after transfer")
      accounts.foreach(acc => miner.wavesBalance(acc).available shouldBe defaultBalance)
      accounts.foreach(acc => miner.wavesBalance(acc).effective shouldBe defaultBalance)
    }

    withClue("beforeAll") {
      correctStartBalancesFuture()
    }
  }
}
