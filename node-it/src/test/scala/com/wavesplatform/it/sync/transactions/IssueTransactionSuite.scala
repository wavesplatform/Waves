package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.api.http.ApiError.{CustomValidationError, InvalidDecimals, InvalidName, NonPositiveAmount}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.IssueTransactionInfo
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.TxVersion
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{JsNull, JsString, JsValue, Json}

class IssueTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {
  test("asset issue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    for (v <- issueTxSupportedVersions) {
      val assetName        = "myasset"
      val assetDescription = "my asset description"
      val (balance1, eff1) = miner.accountBalances(firstAddress)

      val issueTx =
        sender
          .issue(firstKeyPair, assetName, assetDescription, someAssetAmount, 2, reissuable = true, issueFee, version = v, script = scriptText(v))
      nodes.waitForHeightAriseAndTxPresent(issueTx.id)
      if (v > 2) {
        issueTx.chainId shouldBe Some(AddressScheme.current.chainId)
        sender.transactionInfo[IssueTransactionInfo](issueTx.id).chainId shouldBe Some(AddressScheme.current.chainId)
      }

      miner.assertBalances(firstAddress, balance1 - issueFee, eff1 - issueFee)
      miner.assertAssetBalance(firstAddress, issueTx.id, someAssetAmount)
    }
  }

  test("Able to create asset with the same name") {
    for (v <- issueTxSupportedVersions) {
      val assetName        = "myasset1"
      val assetDescription = "my asset description 1"
      val (balance1, eff1) = miner.accountBalances(firstAddress)

      val issuedAssetId =
        sender
          .issue(firstKeyPair, assetName, assetDescription, someAssetAmount, 2, reissuable = false, issueFee, version = v, script = scriptText(v))
          .id
      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      val issuedAssetIdSameAsset =
        sender
          .issue(firstKeyPair, assetName, assetDescription, someAssetAmount, 2, reissuable = true, issueFee, version = v, script = scriptText(v))
          .id
      nodes.waitForHeightAriseAndTxPresent(issuedAssetIdSameAsset)

      miner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
      miner.assertBalances(firstAddress, balance1 - 2 * issueFee, eff1 - 2 * issueFee)
    }
  }

  private def broadcastIssueTxJson(
      source: KeyPair,
      name: String,
      description: String,
      quantity: Long,
      decimals: Int,
      reissuable: Boolean,
      fee: Long,
      scriptOpt: Option[String] = None,
      version: Byte
  ) =
    sender.signedBroadcast(
      Json.obj(
        "type"            -> IssueTransaction.typeId,
        "name"            -> name,
        "quantity"        -> quantity,
        "description"     -> description,
        "sender"          -> source.toAddress,
        "senderPublicKey" -> source.publicKey,
        "decimals"        -> decimals,
        "reissuable"      -> reissuable,
        "fee"             -> fee,
        "version"         -> version,
        "timestamp"       -> System.currentTimeMillis(),
        "proofs"          -> Json.arr(),
        "script"          -> scriptOpt.fold[JsValue](JsNull)(JsString)
      )
    )

  test("Not able to create asset when insufficient funds") {
    for (v <- issueTxSupportedVersions) {
      val assetName        = "myasset"
      val assetDescription = "my asset description"
      val eff1             = miner.accountBalances(firstAddress)._2
      val bigAssetFee      = eff1 + 1.waves

      assertApiError(sender.issue(firstKeyPair, assetName, assetDescription, someAssetAmount, 2, reissuable = false, bigAssetFee, version = v)) {
        error =>
          error.message should include("Accounts balance errors")
      }
    }
  }

  val invalidScript =
    Table(
      ("script", "error"),
      ("base64:AQa3b8tZ", "Invalid checksum"),
      ("base64:", "Can't parse empty script bytes"),
      ("base64:AA==", "Illegal length of script: 1"),
      ("base64:AAQB", "Invalid content type of script: 4"),
      ("base64:AAEF", "Invalid checksum"),
      ("base64:CQEF", "Invalid version of script: 9")
    )

  forAll(invalidScript) { (script: String, error: String) =>
    test(s"Try to put incorrect script=$script") {
      for (v <- issueTxSupportedVersions) {
        val json = {
          val tx = IssueTransaction
            .selfSigned(
              TxVersion.V1,
              firstKeyPair,
              "1234",
              "",
              1,
              2,
              false,
              None,
              issueFee,
              System.currentTimeMillis()
            )
            .explicitGet()
          tx.json() ++ Json.obj("script" -> script)
        }

        assertApiError(
          sender.signedBroadcast(json),
          CustomValidationError(s"ScriptParseError($error)")
        )
      }
    }
  }

  private val invalidAssetValue =
    Table(
      ("assetVal", "decimals", "message"),
      (0L, 2, NonPositiveAmount("0 of assets").assertive(true)),
      (1L, 9, InvalidDecimals("9").assertive()),
      (-1L, 1, NonPositiveAmount("-1 of assets").assertive(true)),
      (1L, -1, InvalidDecimals("-1").assertive())
    )

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, message: AssertiveApiError) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      for (v <- issueTxSupportedVersions) {
        val assetName          = "myasset2"
        val assetDescription   = "my asset description 2"
        val decimalBytes: Byte = decimals.toByte
        assertApiError(
          broadcastIssueTxJson(firstKeyPair, assetName, assetDescription, assetVal, decimalBytes, reissuable = false, issueFee, version = v),
          message
        )
      }
    }
  }

  test(s"Not able to create asset without name") {
    for (v <- issueTxSupportedVersions) {
      assertApiError(sender.issue(firstKeyPair, "", "", someAssetAmount, 2, reissuable = false, issueFee, version = v)) { error =>
        error.message should include("invalid name")
      }
    }
  }

  private val invalid_assets_names =
    Table(
      "abc",
      "UpperCaseAssetCoinTest",
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )

  forAll(invalid_assets_names) { (assetName: String) =>
    test(s"Not able to create asset named $assetName") {
      for (v <- issueTxSupportedVersions) {
        assertApiError(
          broadcastIssueTxJson(firstKeyPair, assetName, assetName, someAssetAmount, 2, reissuable = false, issueFee, version = v),
          InvalidName
        )
      }
    }
  }

  def scriptText(version: Int): Option[String] = version match {
    case 2 => Some(scriptBase64)
    case _ => None
  }

}
