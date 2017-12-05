package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json
import scorex.transaction.lease.LeaseTransaction

import scala.concurrent.Await
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class LeasingTransactionsSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val waitCompletion = 2.minutes

  test("leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    val f = for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)
        .zip(assertBalances(secondAddress, 100.waves, 100.waves))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 10.waves, fee = 10.waves).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(createdLeaseTxId, 1)
      _ <- assertBalances(firstAddress, 90.waves, 80.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough waves") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90.waves, 80.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))

      leaseFailureAssertion <- assertBadRequest(sender.lease(secondAddress, firstAddress, 111.waves, 10.waves))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90.waves, 80.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))
    } yield leaseFailureAssertion

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough waves for fee") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90.waves, 80.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, secondAddress, 90.waves, fee = 11.waves))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90.waves, 80.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }


  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    import LeaseTransaction.Status._
    def status(txId: String) = sender.get(s"/transactions/info/$txId").map { r =>
      (Json.parse(r.getResponseBody) \ "status").as[String]
    }

    val f = for {
      _ <- assertBalances(firstAddress, 90.waves, 80.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 70.waves, fee = 5.waves).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(createdLeaseTxId, 1)
      _ <- assertBalances(firstAddress, 85.waves, 5.waves)
        .zip(assertBalances(secondAddress, 100.waves, 180.waves))

      status1 <- status(createdLeaseTxId)
      _ = assert(status1 == Active.toString)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.waves).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(createdCancelLeaseTxId, 1)
      _ <- assertBalances(firstAddress, 80.waves, 70.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))

      status2 <- status(createdLeaseTxId)
      _ = assert(status2 == Canceled.toString)
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("lease cancellation can be done only once") {
    val f = for {
      _ <- assertBalances(firstAddress, 80.waves, 70.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.waves, fee = 5.waves).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(createdLeaseTxId, 1)
      _ <- assertBalances(firstAddress, 75.waves, 60.waves)
        .zip(assertBalances(secondAddress, 100.waves, 115.waves))

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.waves).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(createdCancelLeaseTxId, 1)
      _ <- assertBadRequest(sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5.waves).map(_.id))
      _ <- assertBalances(firstAddress, 70.waves, 60.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("only sender can cancel lease transaction") {
    val f = for {
      _ <- assertBalances(firstAddress, 70.waves, 60.waves)
        .zip(assertBalances(secondAddress, 100.waves, 110.waves))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5.waves, fee = 5.waves).map(_.id)

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      _ <- traverse(nodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 65.waves, 50.waves)
        .zip(assertBalances(secondAddress, 100.waves, 115.waves))

      _ <- assertBadRequest(sender.cancelLease(thirdAddress, createdLeaseTxId, fee = 1.waves))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough your waves to self") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 65.waves, 50.waves)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, firstAddress, 89.waves, fee = 1.waves))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 65.waves, 50.waves)
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }
}
