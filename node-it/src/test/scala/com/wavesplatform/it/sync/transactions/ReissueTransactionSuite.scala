package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._

class ReissueTransactionSuite extends BaseTransactionSuite {
  import ReissueTransactionSuite._

  test("asset reissue changes issuer's asset balance; issuer's waves balance is decreased by fee") {

    val (balance, effectiveBalance) = miner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, "name2", "description2", someAssetAmount, decimals = 2, reissuable = true, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    miner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
    miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

    val reissueTxId = sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueFee).id
    nodes.waitForHeightAriseAndTxPresent(reissueTxId)
    miner.assertBalances(firstAddress, balance - issueFee - reissueFee, effectiveBalance - issueFee - reissueFee)
    miner.assertAssetBalance(firstAddress, issuedAssetId, 2 * someAssetAmount)
  }

  test("can't reissue not reissuable asset") {
    val (balance, effectiveBalance) = miner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, "name2", "description2", someAssetAmount, decimals = 2, reissuable = false, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)
    miner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
    miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

    assertBadRequestAndMessage(
      sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueFee),
      "Asset is not reissuable"
    )
    nodes.waitForHeightArise()

    miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
    miner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
  }

  test("not able to reissue if cannot pay fee - less than required") {
    val issuedAssetId = sender.issue(firstAddress, "name3", "description3", someAssetAmount, decimals = 2, reissuable = true, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    assertApiError(sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueReducedFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(s"Fee for ReissueTransaction ($reissueReducedFee in WAVES) does not exceed minimal value of $reissueFee WAVES.")

    }
  }

  test("not able to reissue if cannot pay fee - insufficient funds") {

    val (balance, effectiveBalance) = miner.accountBalances(firstAddress)
    val reissueFee                  = effectiveBalance + 1.waves

    val issuedAssetId = sender.issue(firstAddress, "name4", "description4", someAssetAmount, decimals = 2, reissuable = true, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    assertBadRequestAndMessage(
      sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueFee),
      "Accounts balance errors"
    )
    nodes.waitForHeightArise()

    miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
    miner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
  }

  test("accepts reduced fee after feature #16 activation") {
    nodes.waitForHeight(ActivationHeight)

    val issuedAssetId = sender.issue(firstAddress, "name5", "description5", someAssetAmount, decimals = 2, reissuable = true, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    val reissueTxId = sender.reissue(firstAddress, issuedAssetId, someAssetAmount, reissuable = true, fee = reissueReducedFee).id
    nodes.waitForHeightAriseAndTxPresent(reissueTxId)
  }

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((16, ActivationHeight)))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()
}

object ReissueTransactionSuite {
  val ActivationHeight = 15
}
