package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import io.grpc.Status.Code

class ReissueTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime {

  val (reissuer, reissuerAddress) = (firstAcc, firstAddress)

  test("asset reissue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.grpc.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.grpc.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.grpc.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      sender.grpc.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true)

      sender.grpc.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee
      sender.grpc.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee
      sender.grpc.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }

  test("can't reissue not reissuable asset") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.grpc.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.grpc.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.grpc.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = false, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      assertGrpcError(sender.grpc.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, version = v, reissuable = true, waitForTx = true),
        "Asset is not reissuable",
        Code.INVALID_ARGUMENT)

      sender.grpc.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee
      sender.grpc.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee
      sender.grpc.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("not able to reissue if cannot pay fee - insufficient funds") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.grpc.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.grpc.wavesBalance(reissuerAddress).effective
      val hugeReissueFee = reissuerEffBalance + 1.waves

      val issuedAssetTx = sender.grpc.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      assertGrpcError(sender.grpc.broadcastReissue(reissuer, hugeReissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true),
        "negative waves balance",
        Code.INVALID_ARGUMENT)

      sender.grpc.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee
      sender.grpc.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee
      sender.grpc.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("asset becomes non-reissuable after reissue with reissuable=false") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.grpc.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.grpc.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.grpc.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      sender.grpc.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = false, version = v, waitForTx = true)

      assertGrpcError(sender.grpc.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true),
        "Asset is not reissuable",
        Code.INVALID_ARGUMENT)

      sender.grpc.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee
      sender.grpc.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee
      sender.grpc.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }

  test("able to transfer new reissued amount of assets") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.grpc.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.grpc.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.grpc.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      sender.grpc.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true)

      sender.grpc.broadcastTransfer(reissuer, Recipient().withAddress(secondAddress), 2 * someAssetAmount, minFee, assetId = issuedAssetId, waitForTx = true)
      sender.grpc.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee - minFee
      sender.grpc.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee - minFee
      sender.grpc.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L
      sender.grpc.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }


}
