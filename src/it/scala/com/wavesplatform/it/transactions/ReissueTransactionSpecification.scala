package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class ReissueTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses {

  private val defaultQuantity = 100000

  test("asset reissue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)

      issuedAssetId <- sender.issue(firstAddress, "name2", "description2", defaultQuantity, decimals = 2, reissuable = true, fee = 10.waves).map(_.id)

      _ <- waitForHeightAraise(issuedAssetId, 1)

      _ <- assertBalances(firstAddress, 90.waves, 90.waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

      reissuedAssetId <- sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = 10.waves).map(_.id)
      _ <- waitForHeightAraise(issuedAssetId, 1)

      _ <- assertBalances(firstAddress, 80.waves, 80.waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, 2 * defaultQuantity)
    } yield succeed

    Await.result(f, 1.minute)
  }

  // todo can't reissue not reissuable asset

  test("can't reissue not reissuable asset") {
    val issueFee = 10.waves
    val f = for {

      firstAddressBalance <- accountBalance(firstAddress)
      firstAddressEffectiveBalance <- accountEffectiveBalance(firstAddress)

      issuedAssetId <- sender.issue(firstAddress, "name2", "description2", defaultQuantity, decimals = 2, reissuable = false, issueFee).map(_.id)

      _ <- waitForHeightAraise(issuedAssetId, 1)
//70.waves
      _ <- assertBalances(firstAddress, firstAddressBalance - issueFee, firstAddressBalance - issueFee)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

      message = "bla"

      reissuedAssetId <- sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = 10.waves).map(_.id)
      _ <- waitForHeightAraise(issuedAssetId, 1)

//      _ <- assertBadRequestAndMessage( 60.wavws
//        sender.reissue(firstAddress, issuedAssetId, defaultQuantity, reissuable = true, fee = issueFee), message)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
      _ <- assertBalances(firstAddress, firstAddressBalance - issueFee, firstAddressBalance - issueFee)

    } yield succeed

    Await.result(f, 1.minute)
  }


}
