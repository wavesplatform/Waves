package com.wavesplatform.it.sync.grpc

import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.{InvalidName, NonPositiveAmount, TooBigArray}
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

class IssueTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime with TableDrivenPropertyChecks {

  val (issuer,issuerAddress) = (firstAcc,firstAddress)

  test("asset issue changes issuer's asset balance") {
    for (v <- supportedVersions) {
      val assetName        = "myasset"
      val assetDescription = ByteString.copyFrom("my asset description".getBytes(StandardCharsets.UTF_8))
      val issuerBalance = sender.grpc.wavesBalance(issuerAddress).available
      val issuerEffBalance = sender.grpc.wavesBalance(issuerAddress).effective

      val issuedAssetTx = sender.grpc.broadcastIssue(issuer, assetName, someAssetAmount, 8, true, issueFee, assetDescription, version = v, script = scriptText(v), waitForTx =  true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      sender.grpc.wavesBalance(issuerAddress).available shouldBe issuerBalance - issueFee
      sender.grpc.wavesBalance(issuerAddress).effective shouldBe issuerEffBalance - issueFee
      sender.grpc.assetsBalance(issuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("Able to create asset with the same name") {
    for (v <- supportedVersions) {
      val assetName        = "myasset2"
      val assetDescription = ByteString.copyFrom("my asset description 2".getBytes(StandardCharsets.UTF_8))

      val issuedAssetTx = sender.grpc.broadcastIssue(issuer, assetName, someAssetAmount, 8, true, issueFee, assetDescription, version = v, script = scriptText(v), waitForTx =  true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      val issuedAssetTx2 = sender.grpc.broadcastIssue(issuer, assetName, someAssetAmount, 8, true, issueFee, assetDescription, version = v, script = scriptText(v), waitForTx =  true)
      val issuedAssetId2 = PBTransactions.vanilla(issuedAssetTx2).explicitGet().id().base58

      sender.grpc.assetsBalance(issuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
      sender.grpc.assetsBalance(issuerAddress, Seq(issuedAssetId2)).getOrElse(issuedAssetId2, 0L) shouldBe someAssetAmount
    }
  }

  test("Not able to create asset when insufficient funds") {
    val assetName        = "myasset"
    val issuerEffBalance = sender.grpc.wavesBalance(issuerAddress).effective
    val bigAssetFee      = issuerEffBalance + 1.waves

    assertGrpcError(
      sender.grpc.broadcastIssue(issuer, assetName, someAssetAmount, 8, reissuable = false, bigAssetFee),
      "negative waves balance",
      Code.INVALID_ARGUMENT)

  }

  val invalidScript =
    Table(
      ("script", "error"),
      ("base64:AQa3b8tZ", "Invalid checksum"),
      ("base64:", "Can't parse empty script bytes"),
      ("base64:AA==", "Illegal length of script: 1"),
      ("base64:AAQB", "Invalid content type of script: 4"),
      ("base64:AAEE", "Invalid version of script: 4"),
      ("base64:BAEE", "Invalid version of script: 4")
    )

  forAll(invalidScript) { (script: String, error: String) =>
    test(s"Try to put incorrect script=$script") {
      val assetName        = "myasset"
      val assetDescription = "my asset description"

      val thrown = intercept[RuntimeException]{
        sender.grpc.broadcastIssue(issuer, assetName, someAssetAmount, 2, reissuable = true, issueFee, script = Some(script))
      }
      val thrownMessage = thrown.getMessage
      assert(thrownMessage.contains(s"$error"))
    }
  }

  val invalidAssetValue =
    Table(
      ("assetVal", "decimals", "error"),
      (0L, 2, NonPositiveAmount(0,"assets")),
      (1L, 9, TooBigArray),
      (-1L, 1, NonPositiveAmount(-1,"assets")),
      (1L, -1, TooBigArray)
    )

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, error: ValidationError) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      val assetName          = "myasset2"
      val assetDescription   = "my asset description 2"
      val decimalBytes: Byte = decimals.toByte

      val thrown = intercept[RuntimeException]{
        sender.grpc.broadcastIssue(issuer, assetName, assetVal, decimalBytes, false, issueFee)
      }
      val thrownMessage = thrown.getMessage
      assert(thrownMessage.contains(s"$error"))
    }
  }

  val invalid_assets_names =
    Table(
      "abc",
      "UpperCaseAssetCoinTest",
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )

  forAll(invalid_assets_names) { invalidAssetName: String =>
    test(s"Not able to create asset named $invalidAssetName") {
      val thrown = intercept[RuntimeException]{
        sender.grpc.broadcastIssue(issuer, invalidAssetName, someAssetAmount, 2, reissuable = false, issueFee)
      }
      val thrownMessage = thrown.getMessage
      assert(thrownMessage.contains(s"$InvalidName"))
    }
  }

  def scriptText(version: Int): Option[String] = version match {
    case 2 => Some(scriptBase64)
    case _ => None
  }

}
