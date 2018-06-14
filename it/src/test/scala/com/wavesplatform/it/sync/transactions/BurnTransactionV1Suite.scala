package com.wavesplatform.it.sync.transactions

import cats.implicits._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.sync._

class BurnTransactionV1Suite extends BaseTransactionSuite {

  private val decimals: Byte = 2

  test("burning assets changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val (balance, effectiveBalance) = notMiner.accountBalances(firstAddress)
    val issuedAssetId               = sender.issue(firstAddress, "name", "description", issueAmount, decimals, reissuable = false, fee = issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, issueAmount)

    // burn half of the coins and check balance
    val burnId = sender.burn(firstAddress, issuedAssetId, issueAmount / 2, minWavesFee).id

    nodes.waitForHeightAriseAndTxPresent(burnId)
    notMiner.assertBalances(firstAddress, balance - minWavesFee - issueFee, effectiveBalance - minWavesFee - issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, issueAmount / 2)

    val assetOpt = notMiner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
    assert(assetOpt.exists(_.balance == issueAmount / 2))

    // burn the rest and check again
    val burnIdRest = sender.burn(firstAddress, issuedAssetId, issueAmount / 2, minWavesFee).id

    nodes.waitForHeightAriseAndTxPresent(burnIdRest)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)

    val assetOptRest = notMiner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
    assert(assetOptRest.isEmpty)
  }

  test("can burn non-owned asset; issuer asset balance decreased by transfer amount; burner balance decreased by burned amount") {
    val issuedQuantity      = issueAmount
    val transferredQuantity = issuedQuantity / 2
    val burnedQuantity      = transferredQuantity / 2

    val issuedAssetId = sender.issue(firstAddress, "name", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

    val transferId = sender.transfer(firstAddress, secondAddress, transferredQuantity, minWavesFee, issuedAssetId.some).id

    nodes.waitForHeightAriseAndTxPresent(transferId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
    sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

    val burnId = sender.burn(secondAddress, issuedAssetId, burnedQuantity, minWavesFee).id

    nodes.waitForHeightAriseAndTxPresent(burnId)
    sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity - burnedQuantity)
  }

  test("issuer can't burn more tokens than he own") {
    val issuedQuantity = issueAmount
    val burnedQuantity = issuedQuantity * 2

    val issuedAssetId = sender.issue(firstAddress, "name", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

    assertBadRequestAndMessage(sender.burn(secondAddress, issuedAssetId, burnedQuantity, minWavesFee).id, "negative asset balance")
  }

  test("user can't burn more tokens than he own") {
    val issuedQuantity      = issueAmount
    val transferredQuantity = issuedQuantity / 2
    val burnedQuantity      = transferredQuantity * 2

    val issuedAssetId = sender.issue(firstAddress, "name", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

    val transferId = sender.transfer(firstAddress, secondAddress, transferredQuantity, minWavesFee, issuedAssetId.some).id

    nodes.waitForHeightAriseAndTxPresent(transferId)
    sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
    sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

    assertBadRequestAndMessage(sender.burn(secondAddress, issuedAssetId, burnedQuantity, minWavesFee).id, "negative asset balance")
  }
}
