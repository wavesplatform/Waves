package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class LeasingTransactionsSpecification(override val allNodes: Seq[Node]) extends IntegrationSuiteWithThreeAddresses {
  test("leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100 waves, 100 waves)
      _ <- assertBalances(secondAddress, 100 waves, 100 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 10 waves, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 90 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("сan not make leasing without having enough waves") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)

      leaseFailureAssertion <- assertBadRequest(sender.lease(secondAddress, firstAddress, 111 waves, 10 waves))

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)
    } yield leaseFailureAssertion

    Await.result(f, 1 minute)
  }

  test("can not make leasing without having enough waves for fee") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 90 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, secondAddress, 90 waves, fee = 11 waves))

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 90 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }


  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    val f = for {
      _ <- assertBalances(firstAddress, 90 waves, 80 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 70 waves, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 85 waves, 5 waves)
      _ <- assertBalances(secondAddress, 100 waves, 180 waves)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBalances(firstAddress, 80 waves, 70 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("lease cancellation can be done only once") {
    val f = for {
      _ <- assertBalances(firstAddress, 80 waves, 70 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5 waves, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 75 waves, 60 waves)
      _ <- assertBalances(secondAddress, 100 waves, 115 waves)

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdCancelLeaseTxId))

      _ <- assertBadRequest(sender.cancelLease(firstAddress, createdLeaseTxId, fee = 5 waves).map(_.id))

      _ <- assertBalances(firstAddress, 70 waves, 60 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("only sender can cancel lease transaction") {
    val f = for {
      _ <- assertBalances(firstAddress, 70 waves, 60 waves)
      _ <- assertBalances(secondAddress, 100 waves, 110 waves)

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, 5 waves, fee = 5 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- assertBalances(firstAddress, 65 waves, 50 waves)
      _ <- assertBalances(secondAddress, 100 waves, 115 waves)

      _ <- assertBadRequest(sender.cancelLease(thirdAddress, createdLeaseTxId, fee = 1 waves))
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("can not make leasing without having enough your waves to self") {
    val f = for {
      fb <- Future.traverse(allNodes)(_.height).map(_.min)

      _ <- assertBalances(firstAddress, 65 waves, 50 waves)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, firstAddress, 89 waves, fee = 1 waves))

      _ <- Future.traverse(allNodes)(_.waitForHeight(fb + 2))

      _ <- assertBalances(firstAddress, 65 waves, 50 waves)
    } yield transferFailureAssertion

    Await.result(f, 1 minute)
  }
}
