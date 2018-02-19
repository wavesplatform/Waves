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
      (balance, effectiveBalance) <- notMiner.accountBalances(firstAddress)

      issuedAssetId <- sender.issue(firstAddress, "name", "description", defaultQuantity, decimals, reissuable = false, fee = defaultFee).map(_.id)

      _ <- nodes.waitForHeightAraiseAndTxPresent(issuedAssetId)
      _ <- notMiner.assertBalances(firstAddress, balance - defaultFee, effectiveBalance - defaultFee)
        .zip(notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity))

      // burn half of the coins and check balance
      burnId <- sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = defaultFee).map(_.id)

      _ <- nodes.waitForHeightAraiseAndTxPresent(burnId)
      _ <- notMiner.assertBalances(firstAddress, balance - 2 * defaultFee, effectiveBalance - 2 * defaultFee)
        .zip(notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity / 2))
      assetOpt <- notMiner.assetsBalance(firstAddress).map(_.balances.find(_.assetId == issuedAssetId))
      _ = assert(assetOpt.exists(_.balance == defaultQuantity / 2))

      // burn the rest and check again
      burnId <- sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = defaultFee).map(_.id)

      _ <- nodes.waitForHeightAraiseAndTxPresent(burnId)
      _ <- notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)
      assetOpt <- notMiner.assetsBalance(firstAddress).map(_.balances.find(_.assetId == issuedAssetId))
      _ = assert(assetOpt.isEmpty)
    } yield succeed

    Await.result(f, 2.minute)
  }
}
