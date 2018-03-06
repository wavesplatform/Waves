package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._

import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks


class IssueTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  private val defaultQuantity = 100000
  private val assetFee = 5.waves

  test("asset issue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    val assetName = "myasset"
    val assetDescription = "my asset description"
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = true, assetFee).id
    nodes.waitForHeightAraiseAndTxPresent(issuedAssetId)

    notMiner.assertBalances(firstAddress, balance1 - assetFee, eff1 - assetFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
  }

  test("Able to create asset with the same name") {
    val assetName = "myasset1"
    val assetDescription = "my asset description"
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = false, assetFee).id
    nodes.waitForHeightAraiseAndTxPresent(issuedAssetId)

    val issuedAssetIdSameAsset = sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = true, assetFee).id
    nodes.waitForHeightAraiseAndTxPresent(issuedAssetIdSameAsset)

    notMiner.assertAssetBalance(firstAddress, issuedAssetId, defaultQuantity)
    notMiner.assertBalances(firstAddress, balance1 - 2 * assetFee, eff1 - 2 * assetFee)
  }

  test("Not able to create asset when insufficient funds") {
    val assetName = "myasset"
    val assetDescription = "my asset description"
    val eff1 = notMiner.accountBalances(firstAddress)._2
    val bigAssetFee = eff1 + 1.waves

    assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, defaultQuantity, 2, reissuable = false, bigAssetFee),
      "negative waves balance")
  }

  val invalidAssetValue =
    Table(("assetVal", "decimals", "message"),
      (0l, 2, "negative amount"),
      (1l, 9, "Too big sequences requested"),
      (-1l, 1, "negative amount"),
      (1l, -1, "Too big sequences requested"))

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, message: String) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      val assetName = "myasset2"
      val assetDescription = "my asset description 2"
      val decimalBytes: Byte = decimals.toByte
      assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, assetVal, decimalBytes, reissuable = false, assetFee), message)
    }
  }

}
