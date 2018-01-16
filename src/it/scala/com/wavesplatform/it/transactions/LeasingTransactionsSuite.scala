package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json
import scorex.transaction.lease.LeaseTransaction

import scala.concurrent.Await
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class LeasingTransactionsSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val waitCompletion = 2.minutes
  private val defaultFee = 2.waves
  private val leasingAmount = 5.waves

  test("leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    val f = for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdLeaseTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough waves") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      //secondAddress effective balance more than general balance
      leaseFailureAssertion <- assertBadRequest(sender.lease(secondAddress, firstAddress, secondBalance + 1.waves, defaultFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- notMiner.assertBalances(firstAddress, firstBalance, firstEffBalance)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield leaseFailureAssertion

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough waves for fee") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, secondAddress, firstEffBalance, fee = defaultFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- notMiner.assertBalances(firstAddress, firstBalance, firstEffBalance)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }


  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    import LeaseTransaction.Status._
    def status(txId: String) = sender.get(s"/transactions/info/$txId").map { r =>
      (Json.parse(r.getResponseBody) \ "status").as[String]
    }

    val f = for {
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)

      _ <- nodes.waitForHeightAraiseAndTxPresent(createdLeaseTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      status1 <- status(createdLeaseTxId)
      _ = assert(status1 == Active)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdCancelLeaseTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - 2 * defaultFee, firstEffBalance - 2 * defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))

      status2 <- status(createdLeaseTxId)
      _ = assert(status2 == Canceled)
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("lease cancellation can be done only once") {
    val f = for {
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeasingTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdLeasingTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeasingTxId, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdCancelLeaseTxId)
      _ <- assertBadRequest(sender.cancelLease(firstAddress, createdLeasingTxId, fee = defaultFee).map(_.id))
      _ <- notMiner.assertBalances(firstAddress, firstBalance - 2 * defaultFee, firstEffBalance - 2 * defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("only sender can cancel lease transaction") {
    val f = for {
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      _ <- traverse(nodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      _ <- assertBadRequest(sender.cancelLease(thirdAddress, createdLeaseTxId, fee = defaultFee))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough your waves to self") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      (firstBalance, firstEffBalance) <- notMiner.accountBalances(firstAddress)
      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, firstAddress, firstBalance + 1.waves, fee = defaultFee))
      _ <- traverse(nodes)(_.waitForHeight(fb + 2))
      _ <- notMiner.assertBalances(firstAddress, firstBalance, firstEffBalance)
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }

}
