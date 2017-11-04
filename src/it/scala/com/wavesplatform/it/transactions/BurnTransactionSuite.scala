package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._

import scala.concurrent.Await
import scala.concurrent.duration._

class BurnTransactionSuite extends BaseTransactionSuite {

  private val defaultQuantity = 100000

  test("burning assets changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", defaultQuantity, 2, reissuable = false, fee = 1.waves).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId, 1)
      _ <- assertBalances(firstAddress, 99.waves, 99.waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

      burnId <- sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = 1.waves).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(burnId, 1)
      _ <- assertBalances(firstAddress, 98.waves, 98.waves)
      _ <- assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity / 2)

    } yield succeed

    Await.result(f, 1.minute)
  }
}
