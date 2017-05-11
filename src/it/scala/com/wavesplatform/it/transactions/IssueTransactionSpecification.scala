package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class IssueTransactionSpecification(override val allNodes: Seq[Node]) extends IntegrationSuiteWithThreeAddresses {
  test("asset issue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100 waves, 100 waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = true, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 90 waves, 90 waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)
    } yield succeed

    Await.result(f, 1 minute)
  }
}
