package com.wavesplatform.it

import com.wavesplatform.it.api.NodeApi.{AssetBalance, FullAssetInfo, Transaction}
import com.wavesplatform.it.util._
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.transaction.TransactionParser.TransactionType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


trait IntegrationSuiteWithThreeAddresses extends FunSuite with BeforeAndAfterAll with Matchers with ScalaFutures
  with IntegrationPatience with RecoverMethods with RequestErrorAssert with IntegrationTestsScheme {

  def allNodes: Seq[Node]

  def notMiner: Node

  protected val sender: Node = notMiner
  private val richAddress = sender.address

  protected val defaultBalance: Long = 100.waves

  protected lazy val firstAddress: String = Await.result(sender.createAddress, 1.minutes)
  protected lazy val secondAddress: String = Await.result(sender.createAddress, 1.minutes)
  protected lazy val thirdAddress: String = Await.result(sender.createAddress, 1.minutes)

  protected def accountEffectiveBalance(acc: String): Future[Long] = sender.effectiveBalance(acc).map(_.balance)

  protected def accountBalance(acc: String): Future[Long] = sender.balance(acc).map(_.balance)

  protected def assertBalances(acc: String, balance: Long, effectiveBalance: Long): Future[Unit] = {
    for {
      newBalance <- sender.balance(acc).map(_.balance)
      newEffectiveBalance <- sender.effectiveBalance(acc).map(_.balance)
    } yield {
      newEffectiveBalance shouldBe effectiveBalance
      newBalance shouldBe balance
    }
  }

  protected def waitForHeightAraise(transactionId: String, heightIncreaseOn: Integer): Future[Unit] = for {
    height <- traverse(allNodes)(_.height).map(_.max)
    _ <- traverse(allNodes)(_.waitForHeight(height + heightIncreaseOn))
    _ <- traverse(allNodes)(_.waitForTransaction(transactionId))
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


  override def beforeAll(): Unit = {
    super.beforeAll()

    def waitForTxsToReachAllNodes(txIds: Seq[String]): Future[_] = {
      val txNodePairs = for {
        txId <- txIds
        node <- allNodes
      } yield (node, txId)
      Future.traverse(txNodePairs) { case (node, tx) => node.waitForTransaction(tx) }
    }

    def makeTransfers: Future[Seq[String]] = Future.sequence(Seq(
      sender.transfer(richAddress, firstAddress, defaultBalance, sender.fee(TransactionType.TransferTransaction)),
      sender.transfer(richAddress, secondAddress, defaultBalance, sender.fee(TransactionType.TransferTransaction)),
      sender.transfer(richAddress, thirdAddress, defaultBalance, sender.fee(TransactionType.TransferTransaction)))).map(_.map(_.id))

    val correctStartBalancesFuture = for {
      txs <- makeTransfers

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))

      _ <- waitForTxsToReachAllNodes(txs)

      _ <- Future.sequence(Seq(firstAddress, secondAddress, thirdAddress).map(address => assertBalances(address, defaultBalance, defaultBalance)))
    } yield succeed

    Await.result(correctStartBalancesFuture, 2.minutes)
  }
}
