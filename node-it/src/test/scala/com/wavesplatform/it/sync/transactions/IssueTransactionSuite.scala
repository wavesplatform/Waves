package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.it.sync._
import org.scalatest.prop.TableDrivenPropertyChecks

class IssueTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {
  test("asset issue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    for (v <- supportedVersions) {
      val assetName        = "myasset"
      val assetDescription = "my asset description"
      val (balance1, eff1) = miner.accountBalances(firstAddress)

      val issuedAssetId =
        sender
          .issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = true, issueFee, version = v, script = scriptText(v))
          .id
      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      miner.assertBalances(firstAddress, balance1 - issueFee, eff1 - issueFee)
      miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
    }
  }

  test("Able to create asset with the same name") {
    for (v <- supportedVersions) {
      val assetName        = "myasset1"
      val assetDescription = "my asset description 1"
      val (balance1, eff1) = miner.accountBalances(firstAddress)

      val issuedAssetId =
        sender
          .issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = false, issueFee, version = v, script = scriptText(v))
          .id
      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      val issuedAssetIdSameAsset =
        sender
          .issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = true, issueFee, version = v, script = scriptText(v))
          .id
      nodes.waitForHeightAriseAndTxPresent(issuedAssetIdSameAsset)

      miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
      miner.assertBalances(firstAddress, balance1 - 2 * issueFee, eff1 - 2 * issueFee)
    }
  }

  test("Not able to create asset when insufficient funds") {
    val assetName        = "myasset"
    val assetDescription = "my asset description"
    val eff1             = miner.accountBalances(firstAddress)._2
    val bigAssetFee      = eff1 + 1.waves

    assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = false, bigAssetFee),
                               "negative waves balance")
  }

  test("Try to put incorrect script") {
    val assetName        = "myasset"
    val assetDescription = "my asset description"

    assertBadRequestAndMessage(
      sender.issue(firstAddress,
                   assetName,
                   assetDescription,
                   someAssetAmount,
                   2,
                   reissuable = false,
                   issueFee,
                   version = 2,
                   script = Some("base64:AQa3b8tZ")),
      "ScriptParseError(Invalid checksum)"
    )
  }

  val invalidAssetValue =
    Table(
      ("assetVal", "decimals", "message"),
      (0l, 2, "non-positive amount"),
      (1l, 9, "Too big sequences requested"),
      (-1l, 1, "non-positive amount"),
      (1l, -1, "Too big sequences requested")
    )

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, message: String) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      val assetName          = "myasset2"
      val assetDescription   = "my asset description 2"
      val decimalBytes: Byte = decimals.toByte
      assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, assetVal, decimalBytes, reissuable = false, issueFee),
                                 message)
    }
  }

  val invalid_assets_names =
    Table(
      ("abc", "invalid name"),
      (null, "failed to parse json message"),
      ("UpperCaseAssetCoinTest", "invalid name"),
      ("~!|#$%^&*()_+=\";:/?><|\\][{}", "invalid name")
    )

  forAll(invalid_assets_names) { (assetName: String, message: String) =>
    test(s"Not able to create asset named $assetName") {
      assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetName, someAssetAmount, 2, reissuable = false, issueFee), message)
    }
  }

  def scriptText(version: Int) = version match {
    case 2 => Some(scriptBase64)
    case _ => None
  }

}
