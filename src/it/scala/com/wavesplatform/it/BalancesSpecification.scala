package com.wavesplatform.it

import java.io.IOException

import com.wavesplatform.settings.Constants
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers, RecoverMethods}
import scorex.transaction.TransactionParser.TransactionType

import scala.concurrent.Await._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

class BalancesSpecification(allNodes: Seq[Node]) extends FunSuite with Matchers with ScalaFutures
  with IntegrationPatience with BeforeAndAfterAll with RecoverMethods {
  val sender = Random.shuffle(allNodes).head
  val richBalance = sender.address

  var first: String = _
  var second: String = _
  var third: String = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    first = result(sender.createAddress, 5.seconds)
    second = result(sender.createAddress, 5.seconds)
    third = result(sender.createAddress, 5.seconds)

    val f = for {
      txs <- Future.sequence(Seq(
        sender.transfer(richBalance, first, 200 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)),
        sender.transfer(richBalance, second, 100 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)),
        sender.transfer(richBalance, third, 100 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)))
      )
      _ <- {
        val allNodesAllTxsFindInBlockFutures = for {
          tx <- txs
          node <- allNodes
        } yield {
          node.waitForTransaction(tx.id)
        }
        Future.sequence(allNodesAllTxsFindInBlockFutures)
      }

      r1 <- sender.balance(first).map(_.balance == 200 * Constants.UnitsInWave)
      r2 <- sender.balance(second).map(_.balance == 100 * Constants.UnitsInWave)
      r3 <- sender.balance(third).map(_.balance == 100 * Constants.UnitsInWave)
    } yield r1 && r2 && r3
    val res = result(f, 90 seconds)
    res shouldBe true
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

  test("leasing is works correctly") {
    val f = for {
      _ <- assertBalances(first, 200 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)

      createdLeaseTxId <- sender.lease(first, second, 100 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(createdLeaseTxId)))

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield succeed

    result(f, 1 minute)
  }

  test("Сan not make leasing without having enough money") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      leaseFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.lease(second, first, 101 * Constants.UnitsInWave, 10 * Constants.UnitsInWave)
      }

      _ <- Future.sequence(allNodes.map(_.findBlock(_.height >= fb + 2)))

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield leaseFailureAssertion

    result(f, 1 minute)
  }

  test("Can not make transfer without having enough of your own money") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.transfer(second, first, 101 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave)
      }

      _ <- Future.sequence(allNodes.map(_.findBlock(_.height >= fb + 2)))

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield transferFailureAssertion

    result(f, 1 minute)
  }

  test("Can not make transfer without having enough of effective balance") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.transfer(first, second, 91 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave)
      }

      _ <- Future.sequence(allNodes.map(_.findBlock(_.height >= fb + 2)))

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield transferFailureAssertion

    result(f, 1 minute)
  }

  test("Can not make leasing without having enough waves for fee") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferFailureAssertion <- recoverToSucceededIf[IOException] {
        sender.lease(first, second, 88 * Constants.UnitsInWave, fee = 3 * Constants.UnitsInWave)
      }

      _ <- Future.sequence(allNodes.map(_.findBlock(_.height >= fb + 2)))

      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield transferFailureAssertion

    result(f, 1 minute)
  }


  test("leasing cancel is works correctly") {
    val f = for {
      _ <- assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      createdLeaseTxId <- sender.lease(first, second, 70 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(createdLeaseTxId)))

      _ <- assertBalances(first, 180 * Constants.UnitsInWave, 10 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 270 * Constants.UnitsInWave)

      createdCancelLeaseTxId <- sender.leaseCancel(first, createdLeaseTxId, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(createdCancelLeaseTxId)))

      _ <- assertBalances(first, 170 * Constants.UnitsInWave, 70 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
    } yield succeed

    result(f, 1 minute)
  }

  test("Assets issue should not lead to a change in the balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(first, 170 * Constants.UnitsInWave, 70 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(first, "name", "description", 100000, 2, reissuable = true, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(issuedAssetId)))

      _ <- assertBalances(first, 160 * Constants.UnitsInWave, 60 * Constants.UnitsInWave)
      _ <- assertAssetBalance(first, issuedAssetId, 100000)
    } yield succeed

    result(f, 1 minute)
  }

  test("Assets reissue should not lead to a change in the balance and the effective balance only on the fee") {
    val f = for {
      _ <- assertBalances(first, 160 * Constants.UnitsInWave, 60 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(first, "name", "description", 100000, 2, reissuable = true, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(issuedAssetId)))

      reissuedAssetId <- sender.reissue(first, issuedAssetId, 100000, reissuable = true, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(reissuedAssetId)))

      _ <- assertBalances(first, 140 * Constants.UnitsInWave, 40 * Constants.UnitsInWave)
      _ <- assertAssetBalance(first, issuedAssetId, 200000)
    } yield succeed

    result(f, 1 minute)
  }

  test("Assets transfer should not lead to a change in the balance and the effective balance, only on the fee") {
    val f = for {
      _ <- assertBalances(first, 140 * Constants.UnitsInWave, 40 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(first, "name", "description", 100000, 2, reissuable = false, fee = 10 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(issuedAssetId)))

      transferId <- sender.transfer(first, second, 100000, fee = 10 * Constants.UnitsInWave, Some(issuedAssetId)).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(transferId)))

      _ <- assertBalances(first, 120 * Constants.UnitsInWave, 20 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      _ <- assertAssetBalance(first, issuedAssetId, 0)
      _ <- assertAssetBalance(second, issuedAssetId, 100000)
    } yield succeed

    result(f, 1 minute)
  }

  test("Waves transfer should lead to a change in the balance and the effective balance and on the fee") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(first, 120 * Constants.UnitsInWave, 20 * Constants.UnitsInWave)
      _ <- assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)

      transferId <- sender.transfer(first, second, 1 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(transferId)))

      _ <- assertBalances(first, 118 * Constants.UnitsInWave, 18 * Constants.UnitsInWave)
      _ <- assertBalances(second, 101 * Constants.UnitsInWave, 201 * Constants.UnitsInWave)
    } yield succeed

    result(f, 1 minute)
  }

  test("Waves payment should lead to a change in the balance and the effective balance and the fee") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(first, 118 * Constants.UnitsInWave, 18 * Constants.UnitsInWave)
      _ <- assertBalances(second, 101 * Constants.UnitsInWave, 201 * Constants.UnitsInWave)

      transferId <- sender.payment(first, second, 1 * Constants.UnitsInWave, fee = 1 * Constants.UnitsInWave)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(transferId)))

      _ <- assertBalances(first, 116 * Constants.UnitsInWave, 16 * Constants.UnitsInWave)
      _ <- assertBalances(second, 102 * Constants.UnitsInWave, 202 * Constants.UnitsInWave)
    } yield succeed

    result(f, 1 minute)
  }

  test("Burn should not lead to a change in the balance and the effective balance, only on the fee") {
    val f = for {
      fb <- Future.sequence(allNodes.map(_.height)).map(_.min)

      _ <- assertBalances(first, 116 * Constants.UnitsInWave, 16 * Constants.UnitsInWave)

      issuedAssetId <- sender.issue(first, "name", "description", 100000, 2, reissuable = false, fee = 1 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(issuedAssetId)))

      _ <- assertBalances(first, 115 * Constants.UnitsInWave, 15 * Constants.UnitsInWave)
      _ <- assertAssetBalance(first, issuedAssetId, 100000)

      burnId <- sender.burn(first, issuedAssetId, 50000, fee = 1 * Constants.UnitsInWave).map(_.id)

      _ <- Future.sequence(allNodes.map(_.waitForTransaction(burnId)))

      _ <- assertBalances(first, 114 * Constants.UnitsInWave, 14 * Constants.UnitsInWave)

      _ <- assertAssetBalance(first, issuedAssetId, 50000)
    } yield succeed

    result(f, 1 minute)
  }
}
