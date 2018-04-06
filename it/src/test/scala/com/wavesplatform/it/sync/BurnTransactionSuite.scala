package com.wavesplatform.it.sync

import cats.implicits._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._

class BurnTransactionSuite extends BaseTransactionSuite {

  private val defaultQuantity = 100000
  private val decimals: Byte  = 2
  private val defaultFee      = 1.waves

  test("burning assets changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val (balance, effectiveBalance) = notMiner.accountBalances(firstAddress)
    val issuedAssetId               = sender.issue(firstAddress, "name", "description", defaultQuantity, decimals, reissuable = false, fee = defaultFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    notMiner.assertBalances(firstAddress, balance - defaultFee, effectiveBalance - defaultFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)

    // burn half of the coins and check balance
    val burnId = sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = defaultFee).id

    nodes.waitForHeightAriseAndTxPresent(burnId)
    notMiner.assertBalances(firstAddress, balance - 2 * defaultFee, effectiveBalance - 2 * defaultFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity / 2)

    val assetOpt = notMiner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
    assert(assetOpt.exists(_.balance == defaultQuantity / 2))

    // burn the rest and check again
    val burnIdRest = sender.burn(firstAddress, issuedAssetId, defaultQuantity / 2, fee = defaultFee).id

    nodes.waitForHeightAriseAndTxPresent(burnIdRest)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)

    val assetOptRest = notMiner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
    assert(assetOptRest.isEmpty)
  }

  test("can burn non-owned asset; issuer asset balance decreased by transfer amount; burner balance decreased by burned amount") {
    val issuedQuantity      = defaultQuantity
    val transferredQuantity = issuedQuantity / 2
    val burnedQuantity      = transferredQuantity / 2

    val issuedAssetId = sender.issue(firstAddress, "name", "description", issuedQuantity, decimals, reissuable = false, defaultFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

    val transferId = sender.transfer(firstAddress, secondAddress, transferredQuantity, defaultFee, issuedAssetId.some).id

    nodes.waitForHeightAriseAndTxPresent(transferId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
    sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

    val burnId = sender.burn(secondAddress, issuedAssetId, burnedQuantity, defaultFee).id

    nodes.waitForHeightAriseAndTxPresent(burnId)
    sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity - burnedQuantity)
  }

  test("issuer can't burn more tokens than he own") {
    val issuedQuantity = defaultQuantity
    val burnedQuantity = issuedQuantity * 2

    val issuedAssetId = sender.issue(firstAddress, "name", "description", issuedQuantity, decimals, reissuable = false, defaultFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

    assertBadRequestAndMessage(sender.burn(secondAddress, issuedAssetId, burnedQuantity, defaultFee).id, "negative asset balance")
  }

  test("user can't burn more tokens than he own") {
    val issuedQuantity      = defaultQuantity
    val transferredQuantity = issuedQuantity / 2
    val burnedQuantity      = transferredQuantity * 2

    val issuedAssetId = sender.issue(firstAddress, "name", "description", issuedQuantity, decimals, reissuable = false, defaultFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

    val transferId = sender.transfer(firstAddress, secondAddress, transferredQuantity, defaultFee, issuedAssetId.some).id

    nodes.waitForHeightAriseAndTxPresent(transferId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
    sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

    assertBadRequestAndMessage(sender.burn(secondAddress, issuedAssetId, burnedQuantity, defaultFee).id, "negative asset balance")
  }
}
