package com.wavesplatform.it

import java.io.IOException

import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers, RecoverMethods}
import scorex.transaction.TransactionParser.TransactionType
import com.wavesplatform.it.util._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

class BalancesSpecification(allNodes: Seq[Node]) extends FunSuite with Matchers with ScalaFutures
  with IntegrationPatience with BeforeAndAfterAll with RecoverMethods {
  val sender = Random.shuffle(allNodes).head
  val richAddress = sender.address

  private val firstAddress: String = Await.result(sender.createAddress, 1.minute)
  private val secondAddress: String = Await.result(sender.createAddress, 1.minute)
  private val thirdAddress: String = Await.result(sender.createAddress, 1.minute)

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
      sender.transfer(richAddress, firstAddress, 200 waves, sender.fee(TransactionType.TransferTransaction)),
      sender.transfer(richAddress, secondAddress, 100 waves, sender.fee(TransactionType.TransferTransaction)),
      sender.transfer(richAddress, thirdAddress, 100 waves, sender.fee(TransactionType.TransferTransaction)))).map(_.map(_.id))

    val correctStartBalancesFuture = for {
      txs <- makeTransfers

      _ <- waitForTxsToReachAllNodes(txs)

      _ <- assertBalances(firstAddress, 200 waves, 200 waves)
      _ <- assertBalances(secondAddress, 100 waves, 100 waves)
      _ <- assertBalances(thirdAddress, 100 waves, 100 waves)
    } yield succeed

    Await.result(correctStartBalancesFuture, 90 seconds)
  }

  private def assertBalances(acc: String, balance: Long, effectiveBalance: Long): Future[Unit] = {
    for {
      newBalance <- sender.balance(acc).map(_.balance)
      newEffectiveBalance <- sender.effectiveBalance(acc).map(_.balance)
    } yield {
      newEffectiveBalance shouldBe effectiveBalance
      newBalance shouldBe balance
    }
  }

  private def assertAssetBalance(acc: String, assetIdString: String, balance: Long): Future[Unit] = {
    sender.assetBalance(acc, assetIdString).map(_ shouldBe balance)
  }

    test("leasing is working correctly") {
    val f = for {
      _ <- assertBalances(firstAddress, 200 waves, 200 waves)
      _ <- assertBalances(secondAddress, 100 waves, 100 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 100 waves, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("сan not make leasing without having enough waves") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      leaseFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.lease(secondAddress, firstAddress, 101 waves, 10 waves)
      }

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)
    } yield leaseFailureAssertion

    Await.result(f, 1 minute)
  }

  test("can not make transfer without having enough of your own waves") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.transfer(secondAddress, firstAddress, 101 waves, fee = 1 waves)
      }

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }

  test("can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.transfer(firstAddress, secondAddress, 91 waves, fee = 1 waves)
      }

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }

  test("can not make leasing without having enough waves for fee") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.lease(firstAddress, secondAddress, 88 waves, fee = 3 waves)
      }

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }


  test("leasing cancel is working correctly") {
    val f = for {
      _ <- assertBalances(firstAddress, 190 waves, 90 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 70 waves, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 180 waves, 10 waves)
      _ <- assertBalances(secondAddress, 100 waves, 270 waves)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBalances(firstAddress, 170 waves, 70 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("assets issue should lead to a change in the asset balance and to a change of the waves balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 170 waves, 70 waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = true, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 160 waves, 60 waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("assets reissue should lead to a change in the asset balance and a change of the waves balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 160 waves, 60 waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = true, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      reissuedAssetId <- sender.reissue(firstAddress, issuedAssetId, 100000, reissuable = true, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(reissuedAssetId))

      _ <- assertBalances(firstAddress, 140 waves, 40 waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 200000)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("assets transfer should lead to a change in the asset balance and a change of the waves balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 140 waves, 40 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = false, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      transferId <- sender.transfer(firstAddress, secondAddress, 100000, fee = 10 waves, Some(issuedAssetId)).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 120 waves, 20 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, 0)
      _ <- assertAssetBalance(secondAddress, issuedAssetId, 100000)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("waves transfer should lead to a change in the waves balance and the effective balance") {
    val f = for {
      _ <- assertBalances(firstAddress, 120 waves, 20 waves)
      _ <- assertBalances(secondAddress, 100 waves, 200 waves)

      transferId <- sender.transfer(firstAddress, secondAddress, 1 waves, fee = 1 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 118 waves, 18 waves)
      _ <- assertBalances(secondAddress, 101 waves, 201 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("waves payment should lead to a change in the waves balance and the effective balance") {
    val f = for {
      _ <- assertBalances(firstAddress, 118 waves, 18 waves)
      _ <- assertBalances(secondAddress, 101 waves, 201 waves)

      transferId <- sender.payment(firstAddress, secondAddress, 1 waves, fee = 1 waves)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 116 waves, 16 waves)
      _ <- assertBalances(secondAddress, 102 waves, 202 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("assets burn should lead to a change in the asset balance and to a change of the waves balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 116 waves, 16 waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = false, fee = 1 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 115 waves, 15 waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)

      burnId <- sender.burn(firstAddress, issuedAssetId, 50000, fee = 1 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(burnId))

      _ <- assertBalances(firstAddress, 114 waves, 14 waves)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, 50000)
    } yield succeed

    Await.result(f, 1 minute)
  }
}
