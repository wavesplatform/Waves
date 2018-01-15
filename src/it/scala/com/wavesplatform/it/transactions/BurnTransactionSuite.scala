package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._

import scala.concurrent.Await
import scala.concurrent.duration._

class BurnTransactionSuite extends BaseTransactionSuite {

  private val defaultQuantity = 100000
  private val decimals: Byte = 2
  private val defaultFee = 1.waves

  test("burning assets changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val f = for {
      (balance, effectiveBalance) <- accountBalances(firstAddress)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", defaultQuantity, decimals, reissuable = false, fee = defaultFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(issuedAssetId)
      _ <- assertBalances(firstAddress, balance - defaultFee, effectiveBalance - defaultFee)
        .zip(assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity))

      burnId <- sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = defaultFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(burnId)
      _ <- assertBalances(firstAddress, balance - 2 * defaultFee, effectiveBalance - 2 * defaultFee)
        .zip(assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity / 2))

    } yield succeed

    Await.result(f, 2.minute)
  }
}
