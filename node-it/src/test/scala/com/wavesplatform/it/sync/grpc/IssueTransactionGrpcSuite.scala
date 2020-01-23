package com.wavesplatform.it.sync.grpc

import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.{InvalidName, NonPositiveAmount, TooBigArray}
import com.wavesplatform.transaction.assets.IssueTransaction
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random

class IssueTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime with TableDrivenPropertyChecks {

  val (issuer,issuerAddress) = (firstAcc,firstAddress)

  test("asset issue changes issuer's asset balance") {
    for (v <- supportedVersions) {
      val assetName        = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MinAssetNameLength).mkString
      val assetDescription = ByteString.copyFrom("my asset description".getBytes(StandardCharsets.UTF_8))
      val issuerBalance = sender.wavesBalance(issuerAddress).available
      val issuerEffBalance = sender.wavesBalance(issuerAddress).effective

      val issuedAssetTx = sender.broadcastIssue(issuer, assetName, someAssetAmount, 8, reissuable = true, issueFee, assetDescription, version = v, script = scriptText(v), waitForTx =  true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      sender.wavesBalance(issuerAddress).available shouldBe issuerBalance - issueFee
      sender.wavesBalance(issuerAddress).effective shouldBe issuerEffBalance - issueFee
      sender.assetsBalance(issuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount

      val assetInfo = sender.getTransaction(issuedAssetId).getTransaction.getIssue

      assetInfo.decimals shouldBe 8
      assetInfo.amount shouldBe someAssetAmount
      assetInfo.reissuable shouldBe true
      assetInfo.description shouldBe assetDescription

    }
  }

  test("not able to issue asset with fee less then issueFee (minFee for NFT)") {
    for (v <- supportedVersions) {
      val assetName = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MinAssetNameLength + 1).mkString
      val assetDescription = ByteString.copyFrom("nft asset".getBytes(StandardCharsets.UTF_8))
      val issuerBalance = sender.wavesBalance(issuerAddress).available
      val issuerEffBalance = sender.wavesBalance(issuerAddress).effective
      val (nftQuantity, nftDecimals, nftReissuable) = (1, 0, false)

      assertGrpcError(
        sender.broadcastIssue(issuer, assetName, someAssetAmount, 7, reissuable = true, issueFee - 1, assetDescription, version = v),
        s"does not exceed minimal value of $issueFee",
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        sender.broadcastIssue(issuer, assetName, nftQuantity, nftDecimals, nftReissuable, minFee - 1, assetDescription, version = v),
        s"does not exceed minimal value of $minFee",
        Code.INVALID_ARGUMENT
      )

      sender.wavesBalance(issuerAddress).available shouldBe issuerBalance
      sender.wavesBalance(issuerAddress).effective shouldBe issuerEffBalance
    }
  }

  test("Able to create asset with the same name") {
    for (v <- supportedVersions) {
      val assetName        = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MaxAssetNameLength).mkString
      val assetDescription = ByteString.copyFrom("my asset description 2".getBytes(StandardCharsets.UTF_8))

      val issuedAssetTx = sender.broadcastIssue(issuer, assetName, someAssetAmount, 7, reissuable = true, issueFee, assetDescription, version = v, script = scriptText(v), waitForTx =  true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      val issuedAssetTx2 = sender.broadcastIssue(issuer, assetName, someAssetAmount, 7, reissuable = true, issueFee, assetDescription, version = v, script = scriptText(v), waitForTx =  true)
      val issuedAssetId2 = PBTransactions.vanilla(issuedAssetTx2).explicitGet().id().base58

      sender.assetsBalance(issuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
      sender.assetsBalance(issuerAddress, Seq(issuedAssetId2)).getOrElse(issuedAssetId2, 0L) shouldBe someAssetAmount

      sender.getTransaction(issuedAssetId).getTransaction.getIssue.name shouldBe ByteString.copyFrom(assetName.getBytes(StandardCharsets.UTF_8))
      sender.getTransaction(issuedAssetId2).getTransaction.getIssue.name shouldBe ByteString.copyFrom(assetName.getBytes(StandardCharsets.UTF_8))
    }
  }

  test("Not able to create asset when insufficient funds") {
    val assetName        = "myasset"
    val issuerEffBalance = sender.wavesBalance(issuerAddress).effective
    val bigAssetFee      = issuerEffBalance + 1.waves

    assertGrpcError(
      sender.broadcastIssue(issuer, assetName, someAssetAmount, 8, reissuable = false, bigAssetFee),
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
      
      assertGrpcError(
      sender.broadcastIssue(issuer, assetName, someAssetAmount, 2, reissuable = true, issueFee, script = Some(script)),
        error,
        Code.INTERNAL
      )
    }
  }

  val invalidAssetValue =
    Table(
      ("assetVal", "decimals", "message"),
      (0L, 2, "NonPositiveAmount"),
      (1L, IssueTransaction.MaxDecimals + 1, "TooBigArray"),
      (-1L, 1, "NonPositiveAmount"),
      (1L, -1, "TooBigArray")
    )

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, error: String) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      val assetName          = "myasset2"
      val decimalBytes: Byte = decimals.toByte
      assertGrpcError(
      sender.broadcastIssue(issuer, assetName, assetVal, decimalBytes, reissuable = false, issueFee),
        s"$error",
        Code.INTERNAL
      )
    }
  }
  val tooSmallAssetName = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MinAssetNameLength - 1).mkString
  val tooBigAssetName = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MaxAssetNameLength + 1).mkString
  val invalid_assets_names =
    Table(
      tooSmallAssetName,
      tooBigAssetName,
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )

  forAll(invalid_assets_names) { invalidAssetName: String =>
    test(s"Not able to create asset named $invalidAssetName") {
      assertGrpcError(
        sender.broadcastIssue(issuer, invalidAssetName, someAssetAmount, 2, reissuable = false, issueFee),
        s"$InvalidName",
        Code.INTERNAL
      )
    }
  }

  test("Not able to create asset with too big description") {
    val tooBigDescription = ByteString.copyFrom(Random.nextString(IssueTransaction.MaxDescriptionLength + 1).getBytes)
    assertGrpcError(
      sender.broadcastIssue(issuer, "assetName", someAssetAmount, 2, description = tooBigDescription, reissuable = false, fee = issueFee),
      s"$TooBigArray",
      Code.INTERNAL
    )
  }

  def scriptText(version: Int): Option[String] = version match {
    case 2 => Some(scriptBase64)
    case _ => None
  }

}
