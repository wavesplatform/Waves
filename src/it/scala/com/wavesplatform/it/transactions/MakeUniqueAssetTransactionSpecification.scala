package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class MakeUniqueAssetTransactionSpecification(override val allNodes: Seq[Node]) extends IntegrationSuiteWithThreeAddresses {
  test("make unique assets transaction makes asset name unique: it's impossible to issue the new one with this name") {
    val f = for {
      _ <- assertBalances(firstAddress, 100 waves, 100 waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = true, fee = 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(issuedAssetId))

      _ <- assertBalances(firstAddress, 90 waves, 90 waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)

      makeUnique <- sender.makeUnique(firstAddress, issuedAssetId, 10 waves).map(_.id)

      _ <- Future.traverse(allNodes)(_.waitForTransaction(makeUnique))

      _ <- assertBalances(firstAddress, 80 waves, 80 waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 100000)

      _ <- assertRequestError(sender.issue(firstAddress, "name", "description", 100000, 2, reissuable = true, fee = 10 waves))
    } yield succeed

    Await.result(f, 1 minute)
  }
}
