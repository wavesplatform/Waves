package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.assets.IssueTransaction
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random

class IssueTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime with TableDrivenPropertyChecks {

  val (issuer, issuerAddress) = (firstAcc, firstAddress)

  test("asset issue changes issuer's asset balance") {
    for (v <- issueTxSupportedVersions) {
      val assetName        = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MinAssetNameLength).mkString
      val assetDescription = "my asset description"
      val issuerBalance    = miner.wavesBalance(issuerAddress).available
      val issuerEffBalance = miner.wavesBalance(issuerAddress).effective

      val issuedAssetTx = miner.broadcastIssue(
        issuer,
        assetName,
        someAssetAmount,
        8,
        reissuable = true,
        issueFee,
        assetDescription,
        version = v,
        script = scriptText(v),
        waitForTx = true
      )
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      miner.wavesBalance(issuerAddress).available shouldBe issuerBalance - issueFee
      miner.wavesBalance(issuerAddress).effective shouldBe issuerEffBalance - issueFee
      miner.assetsBalance(issuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount

      val assetInfo = miner.getTransaction(issuedAssetId).getTransaction.getIssue

      assetInfo.decimals shouldBe 8
      assetInfo.amount shouldBe someAssetAmount
      assetInfo.reissuable shouldBe true
      assetInfo.description shouldBe assetDescription

    }
  }

  test("not able to issue asset with fee less then issueFee (minFee for NFT)") {
    for (v <- issueTxSupportedVersions) {
      val assetName                                 = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MinAssetNameLength + 1).mkString
      val assetDescription                          = "nft asset"
      val issuerBalance                             = miner.wavesBalance(issuerAddress).available
      val issuerEffBalance                          = miner.wavesBalance(issuerAddress).effective
      val (nftQuantity, nftDecimals, nftReissuable) = (1, 0, false)

      assertGrpcError(
        miner.broadcastIssue(issuer, assetName, someAssetAmount, 7, reissuable = true, issueFee - 1, assetDescription, version = v),
        s"does not exceed minimal value of $issueFee",
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        miner.broadcastIssue(issuer, assetName, nftQuantity, nftDecimals, nftReissuable, minFee - 1, assetDescription, version = v),
        s"does not exceed minimal value of $minFee",
        Code.INVALID_ARGUMENT
      )

      miner.wavesBalance(issuerAddress).available shouldBe issuerBalance
      miner.wavesBalance(issuerAddress).effective shouldBe issuerEffBalance
    }
  }

  test("Able to create asset with the same name") {
    for (v <- issueTxSupportedVersions) {
      val assetName        = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MaxAssetNameLength).mkString
      val assetDescription = "my asset description 2"

      val issuedAssetTx = miner.broadcastIssue(
        issuer,
        assetName,
        someAssetAmount,
        7,
        reissuable = true,
        issueFee,
        assetDescription,
        version = v,
        script = scriptText(v),
        waitForTx = true
      )
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      val issuedAssetTx2 = miner.broadcastIssue(
        issuer,
        assetName,
        someAssetAmount,
        7,
        reissuable = true,
        issueFee,
        assetDescription,
        version = v,
        script = scriptText(v),
        waitForTx = true
      )
      val issuedAssetId2 = PBTransactions.vanilla(issuedAssetTx2).explicitGet().id().toString

      miner.assetsBalance(issuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
      miner.assetsBalance(issuerAddress, Seq(issuedAssetId2)).getOrElse(issuedAssetId2, 0L) shouldBe someAssetAmount

      miner.getTransaction(issuedAssetId).getTransaction.getIssue.name shouldBe assetName
      miner.getTransaction(issuedAssetId2).getTransaction.getIssue.name shouldBe assetName
    }
  }

  test("Not able to create asset when insufficient funds") {
    val assetName        = "myasset"
    val issuerEffBalance = miner.wavesBalance(issuerAddress).effective
    val bigAssetFee      = issuerEffBalance + 1.waves

    assertGrpcError(
      miner.broadcastIssue(issuer, assetName, someAssetAmount, 8, reissuable = false, bigAssetFee),
      "Accounts balance errors",
      Code.INVALID_ARGUMENT
    )

  }

  val invalidScript =
    Table(
      ("script", "error"),
      ("base64:AQa3b8tZ", "Invalid checksum"),
      ("base64:AA==", "Illegal length of script: 1"),
      ("base64:AAQB", "Invalid content type of script: 4"),
      ("base64:AAEF", "Invalid checksum"),
      ("base64:CAEF", "Invalid version of script: 8")
    )

  forAll(invalidScript) { (script: String, error: String) =>
    test(s"Try to put incorrect script=$script") {
      val assetName = "myasset"

      assertGrpcError(
        miner.broadcastIssue(issuer, assetName, someAssetAmount, 2, reissuable = true, issueFee, script = Left(Base64.decode(script))),
        error,
        Code.INVALID_ARGUMENT
      )
    }
  }

  val invalidAssetValue =
    Table(
      ("assetVal", "decimals", "message"),
      (0L, 2, "non-positive amount"),
      (1L, IssueTransaction.MaxAssetDecimals + 1, "Too big sequence requested"),
      (-1L, 1, "non-positive amount"),
      (1L, -1, "Too big sequence requested")
    )

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, error: String) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      val assetName          = "myasset2"
      val decimalBytes: Byte = decimals.toByte
      assertGrpcError(
        miner.broadcastIssue(issuer, assetName, assetVal, decimalBytes, reissuable = false, issueFee),
        s"$error",
        Code.INVALID_ARGUMENT
      )
    }
  }
  val tooSmallAssetName = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MinAssetNameLength - 1).mkString
  val tooBigAssetName   = Random.alphanumeric.filter(_.isLetter).take(IssueTransaction.MaxAssetNameLength + 1).mkString
  val invalid_assets_names =
    Table(
      tooSmallAssetName,
      tooBigAssetName,
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )

  forAll(invalid_assets_names) { invalidAssetName: String =>
    test(s"Not able to create asset named $invalidAssetName") {
      assertGrpcError(
        miner.broadcastIssue(issuer, invalidAssetName, someAssetAmount, 2, reissuable = false, issueFee),
        "invalid name",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("Not able to create asset with too big description") {
    val tooBigDescription = Random.nextString(1000 + 1)
    assertGrpcError(
      miner.broadcastIssue(issuer, "assetName", someAssetAmount, 2, description = tooBigDescription, reissuable = false, fee = issueFee),
      "Too big sequence requested",
      Code.INVALID_ARGUMENT
    )
  }

  def scriptText(version: Int): Either[Array[Byte], Option[Script]] = Right(version match {
    case 2 => Some(script)
    case _ => None
  })
}
