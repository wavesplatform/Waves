package com.wavesplatform.it

import com.wavesplatform.it.api.MultipleNodesApi
import com.wavesplatform.it.api.NodeApi.{AssetBalance, FullAssetInfo}
import com.wavesplatform.it.util._
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.transaction.TransactionParser.TransactionType
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait IntegrationSuiteWithThreeAddresses extends BeforeAndAfterAll with Matchers with ScalaFutures
  with IntegrationPatience with RecoverMethods with RequestErrorAssert with IntegrationTestsScheme
  with MultipleNodesApi with ScorexLogging {
  this: Suite =>

  def nodes: Seq[Node]
  def notMiner: Node

  protected def sender: Node = notMiner
  private def richAddress = sender.address

  protected val defaultBalance: Long = 100.waves

  protected lazy val firstAddress: String = Await.result(sender.createAddress, 1.minutes)
  protected lazy val secondAddress: String = Await.result(sender.createAddress, 1.minutes)
  protected lazy val thirdAddress: String = Await.result(sender.createAddress, 1.minutes)

  protected def accountEffectiveBalance(acc: String): Future[Long] = sender.effectiveBalance(acc).map(_.balance)

  protected def accountBalance(acc: String): Future[Long] = sender.balance(acc).map(_.balance)

  protected def assertBalances(acc: String, balance: Long, effectiveBalance: Long): Future[Unit] = {
    for {
      newBalance <- accountBalance(acc)
      newEffectiveBalance <- accountEffectiveBalance(acc)
    } yield {
      withClue(s"effective balance of $acc") {
        newEffectiveBalance shouldBe effectiveBalance
      }
      withClue(s"balance of $acc") {
        newBalance shouldBe balance
      }
    }
  }

  protected def dumpBalances(node: Node, accounts: Seq[String], label: String): Future[Unit] = {
    Future
      .traverse(accounts) { acc =>
        accountBalance(acc).zip(accountEffectiveBalance(acc)).map(acc -> _)
      }
      .map { info =>
        val formatted = info
          .map { case (account, (balance, effectiveBalance)) =>
            f"$account: balance = $balance, effective = $effectiveBalance"
          }
          .mkString("\n")
        log.debug(s"$label:\n$formatted")
      }
  }

  // if we first await tx and then height + 1, it could be gone with height + 1
  // if we first await height + 1 and then tx, it could be gone with height + 2
  // so we await tx twice
  protected def waitForHeightAraiseAndTxPresent(transactionId: String, heightIncreaseOn: Integer): Future[Unit] = for {
    height <- traverse(nodes)(_.height).map(_.max)
    _ <- traverse(nodes)(_.waitForTransaction(transactionId))
    _ <- traverse(nodes)(_.waitForHeight(height + heightIncreaseOn))
    _ <- traverse(nodes)(_.waitForTransaction(transactionId))
  } yield ()

  protected def waitForHeightAraise(heightIncreaseOn: Integer): Future[Unit] = for {
    height <- traverse(nodes)(_.height).map(_.max)
    _ <- traverse(nodes)(_.waitForHeight(height + heightIncreaseOn))
  } yield ()


  protected def assertAssetBalance(acc: String, assetIdString: String, balance: Long): Future[Unit] = {
    assertAsset(acc, assetIdString)(_.balance shouldBe balance)
  }

  protected def assertAsset(acc: String, assetIdString: String)(assertion: AssetBalance => Unit): Future[Unit] = {
    sender.assetBalance(acc, assetIdString).map(assertion)
  }

  protected def assertFullAssetInfo(acc: String, assetIdString: String)(assertion: FullAssetInfo => Unit): Future[Unit] = {
    sender.assetsBalance(acc).map(fasi => {
      val maybeAssetInfo = fasi.balances.find(_.assetId == assetIdString)
      maybeAssetInfo.isEmpty shouldBe false
      assertion(maybeAssetInfo.get)
    })
  }

  abstract override def beforeAll(): Unit = {
    super.beforeAll()

    def waitForTxsToReachAllNodes(txIds: Seq[String]): Future[_] = {
      val txNodePairs = for {
        txId <- txIds
        node <- nodes
      } yield (node, txId)
      traverse(txNodePairs) { case (node, tx) => node.waitForTransaction(tx) }
    }

    val accounts = Seq(firstAddress, secondAddress, thirdAddress)

    def makeTransfers: Future[Seq[String]] = traverse(accounts) { acc =>
      sender.transfer(richAddress, acc, defaultBalance, sender.fee(TransactionType.TransferTransaction)).map(_.id)
    }

    val correctStartBalancesFuture = for {
      _ <- dumpBalances(sender, accounts, "initial")
      txs <- makeTransfers

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 2))

      _ <- waitForTxsToReachAllNodes(txs)
      _ <- dumpBalances(sender, accounts, "after transfer")
      _ <- traverse(accounts)(assertBalances(_, defaultBalance, defaultBalance))
    } yield succeed

    Await.result(correctStartBalancesFuture, 5.minutes)
  }
}
