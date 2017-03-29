package com.wavesplatform.it

import java.io.IOException

import com.wavesplatform.settings.Constants
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers, RecoverMethods}
import scorex.transaction.TransactionParser.TransactionType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Random

class BalancesSpecification(allNodes: Seq[Node]) extends FunSuite with Matchers with ScalaFutures
  with IntegrationPatience with BeforeAndAfterAll with RecoverMethods {
  val sender = Random.shuffle(allNodes).head
  val richAddress = sender.address

  var firstAddress: String = _
  var secondAddress: String = _
  var thirdAddress: String = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    firstAddress = Await.result(sender.createAddress, 5.seconds)
    secondAddress = Await.result(sender.createAddress, 5.seconds)
    thirdAddress = Await.result(sender.createAddress, 5.seconds)

    def waitAllTxsFindedInAllNodes(txIds: Seq[String]): Future[_] = {
      val txNodePairs = for {
        txId <- txIds
        node <- allNodes
      } yield {
        (node, txId)
      }
      Future.traverse(txNodePairs) { case (node, tx) => node.waitForTransaction(tx) }
    }

    def makeTransfers: Future[Seq[String]] = Future.sequence(Seq(
      sender.transfer(richAddress, firstAddress, 200 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)),
      sender.transfer(richAddress, secondAddress, 100 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)),
      sender.transfer(richAddress, thirdAddress, 100 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)))).map(_.map(_.id))

    val correctStartBalancesFuture = for {
      txs <- makeTransfers

      _ <- waitAllTxsFindedInAllNodes(txs)

      r1 <- sender.balance(firstAddress).map(_.balance == 200 * Constants.UnitsInWave)
      r2 <- sender.balance(secondAddress).map(_.balance == 100 * Constants.UnitsInWave)
      r3 <- sender.balance(thirdAddress).map(_.balance == 100 * Constants.UnitsInWave)
    } yield r1 && r2 && r3

    val correctStartBalances = Await.result(correctStartBalancesFuture, 90 seconds)
    correctStartBalances shouldBe true
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
      _ <- assertBalances(firstAddress, 200 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 100 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(createdLeaseTxId)))

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Сan not make leasing without having enough money") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      leaseFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.lease(secondAddress, firstAddress, 101 * Constants.UnitsInWave, 10 * Constants.UnitsInWave)
      }

      _ <- Future.sequence(allNodes.map(_.findBlock(_.height >= fb + 2)))

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield leaseFailureAssertion

    Await.result(f, 1 minute)
  }

  test("Can not make transfer without having enough of your own money") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.transfer(secondAddress, firstAddress, 101 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave)
      }

      _ <- Future.sequence(allNodes.map(_.findBlock(_.height >= fb + 2)))

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }

  test("Can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.transfer(firstAddress, secondAddress, 91 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave)
      }

      _ <- Future.sequence(allNodes.map(_.findBlock(_.height >= fb + 2)))

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }

  test("Can not make leasing without having enough waves for fee") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.lease(firstAddress, secondAddress, 88 * Constants.UnitsInWave, fee = 3 * Constants.UnitsInWave)
      }

      _ <- Future.traverse(allNodes)(_.findBlock(_.height >= fb + 2))

      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }


  test("leasing cancel is works correctly") {
    val f = for {
      _ <- assertBalances(firstAddress, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 70 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 180 * Constants.UnitsInWave, 10 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 270 * Constants.UnitsInWave)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBalances(firstAddress, 170 * Constants.UnitsInWave, 70 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Assets issue should not lead to a change in the balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 170 * Constants.UnitsInWave, 70 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = true, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 160 * Constants.UnitsInWave, 60 * Constants.UnitsInWave)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Assets reissue should not lead to a change in the balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 160 * Constants.UnitsInWave, 60 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = true, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      reissuedAssetId <- sender.reissue(firstAddress, issuedAssetId, 100000, reissuable = true, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(reissuedAssetId))

      _ <- assertBalances(firstAddress, 140 * Constants.UnitsInWave, 40 * Constants.UnitsInWave)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 200000)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Assets transfer should not lead to a change in the balance and the effective balance, only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 140 * Constants.UnitsInWave, 40 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = false, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      transferId <- sender.transfer(firstAddress, secondAddress, 100000, fee = 10 * Constants.UnitsInWave, Some(issuedAssetId)).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 120 * Constants.UnitsInWave, 20 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, 0)
      _ <- assertAssetBalance(secondAddress, issuedAssetId, 100000)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Waves transfer should lead to a change in the balance and the effective balance and on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 120 * Constants.UnitsInWave, 20 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferId <- sender.transfer(firstAddress, secondAddress, 1 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 118 * Constants.UnitsInWave, 18 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 101 * Constants.UnitsInWave, 201 * Constants.UnitsInWave)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Waves payment should lead to a change in the balance and the effective balance and the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 118 * Constants.UnitsInWave, 18 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 101 * Constants.UnitsInWave, 201 * Constants.UnitsInWave)

      transferId <- sender.payment(firstAddress, secondAddress, 1 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 116 * Constants.UnitsInWave, 16 * Constants.UnitsInWave)
      _ <- assertBalances(secondAddress, 102 * Constants.UnitsInWave, 202 * Constants.UnitsInWave)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Burn should not lead to a change in the balance and the effective balance, only on the fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 116 * Constants.UnitsInWave, 16 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = false, fee = 1 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 115 * Constants.UnitsInWave, 15 * Constants.UnitsInWave)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)

      burnId <- sender.burn(firstAddress, issuedAssetId, 50000, fee = 1 * Constants.UnitsInWave).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(burnId))

      _ <- assertBalances(firstAddress, 114 * Constants.UnitsInWave, 14 * Constants.UnitsInWave)

      _ <- assertAssetBalance(firstAddress, issuedAssetId, 50000)
    } yield succeed

    Await.result(f, 1 minute)
  }
}
