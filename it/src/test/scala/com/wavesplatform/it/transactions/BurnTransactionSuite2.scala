package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._

class BurnTransactionSuite2 extends BaseTransactionSuite {

  private val defaultQuantity = 100000
  private val decimals: Byte = 2
  private val defaultFee = 1.waves

  test("burning assets changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val (balance, effectiveBalance) = notMiner.accountBalances(firstAddress)
    val issuedAssetId = sender.issue(firstAddress, "name", "description", defaultQuantity, decimals, reissuable = false, fee = defaultFee).id

    nodes.waitForHeightAraiseAndTxPresent(issuedAssetId)
    notMiner.assertBalances(firstAddress, balance - defaultFee, effectiveBalance - defaultFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

    // burn half of the coins and check balance
    var burnId = sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = defaultFee).id

    nodes.waitForHeightAraiseAndTxPresent(burnId)
    notMiner.assertBalances(firstAddress, balance - 2 * defaultFee, effectiveBalance - 2 * defaultFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity / 2)

    // burn the rest and check again
    burnId = sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = defaultFee).id

    nodes.waitForHeightAraiseAndTxPresent(burnId)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)

  }
}
