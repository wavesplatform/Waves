package com.wavesplatform.it

import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.transaction.transfer._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

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

  def notMiner: Node

  protected def sender: Node = notMiner

  protected lazy val firstAddress: String  = Await.result(sender.createAddress, 2.minutes)
  protected lazy val secondAddress: String = Await.result(sender.createAddress, 2.minutes)
  protected lazy val thirdAddress: String  = Await.result(sender.createAddress, 2.minutes)

  def pkByAddress(address: String) = PrivateKeyAccount.fromSeed(Await.result(sender.seed(address), 10.seconds)).right.get

  abstract protected override def beforeAll(): Unit = {
    super.beforeAll()

    val defaultBalance: Long = 100.waves

    def dumpBalances(node: Node, accounts: Seq[String], label: String): Future[Unit] = {
      Future
        .traverse(accounts) { acc =>
          notMiner.accountBalance(acc).zip(notMiner.accountEffectiveBalance(acc)).map(acc -> _)
        }
        .map { info =>
          val formatted = info
            .map {
              case (account, (balance, effectiveBalance)) =>
                f"$account: balance = $balance, effective = $effectiveBalance"
            }
            .mkString("\n")
          log.debug(s"$label:\n$formatted")
        }
    }

    def waitForTxsToReachAllNodes(txIds: Seq[String]): Future[_] = {
      val txNodePairs = for {
        txId <- txIds
        node <- nodes
      } yield (node, txId)
      traverse(txNodePairs) { case (node, tx) => node.waitForTransaction(tx) }
    }

    def makeTransfers(accounts: Seq[String]): Future[Seq[String]] = traverse(accounts) { acc =>
      sender.transfer(sender.address, acc, defaultBalance, sender.fee(TransferTransactionV1.typeId)).map(_.id)
    }

    val correctStartBalancesFuture = for {
      _ <- traverse(nodes)(_.waitForHeight(2))
      accounts = Seq(firstAddress, secondAddress, thirdAddress)

      _   <- dumpBalances(sender, accounts, "initial")
      txs <- makeTransfers(accounts)

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- withClue(s"waitForHeight(${height + 2})") {
        Await.ready(traverse(nodes)(_.waitForHeight(height + 2)), 3.minutes)
      }

      _ <- withClue("waitForTxsToReachAllNodes") {
        Await.ready(waitForTxsToReachAllNodes(txs), 2.minute)
      }
      _ <- dumpBalances(sender, accounts, "after transfer")
      _ <- traverse(accounts)(notMiner.assertBalances(_, defaultBalance, defaultBalance))
    } yield succeed

    withClue("beforeAll") {
      Await.result(correctStartBalancesFuture, 5.minutes)
    }
  }
}
