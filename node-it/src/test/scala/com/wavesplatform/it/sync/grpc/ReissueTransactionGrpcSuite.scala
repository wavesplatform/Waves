package com.wavesplatform.it.sync.grpc

import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.sync._
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.it.util._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.Recipient
import io.grpc.Status.Code

class ReissueTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime {

  val (reissuer, reissuerAddress) = (firstAcc, firstAddress)

  test("asset reissue changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      sender.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true)

      sender.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee
      sender.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee
      sender.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }

  test("can't reissue not reissuable asset") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = false, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      assertGrpcError(sender.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, version = v, reissuable = true, waitForTx = true),
        "Asset is not reissuable",
        Code.INVALID_ARGUMENT)

      sender.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee
      sender.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee
      sender.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("not able to reissue if cannot pay fee - insufficient funds") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.wavesBalance(reissuerAddress).effective
      val hugeReissueFee = reissuerEffBalance + 1.waves

      val issuedAssetTx = sender.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      assertGrpcError(sender.broadcastReissue(reissuer, hugeReissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true),
        "negative waves balance",
        Code.INVALID_ARGUMENT)

      sender.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee
      sender.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee
      sender.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("asset becomes non-reissuable after reissue with reissuable=false") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      sender.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = false, version = v, waitForTx = true)

      assertGrpcError(sender.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true),
        "Asset is not reissuable",
        Code.INVALID_ARGUMENT)

      sender.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee
      sender.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee
      sender.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }

  test("able to transfer new reissued amount of assets") {
    for (v <- supportedVersions) {
      val reissuerBalance = sender.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = sender.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = sender.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().base58

      sender.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true)

      sender.broadcastTransfer(reissuer, Recipient().withAddress(secondAddress), 2 * someAssetAmount, minFee, assetId = issuedAssetId, waitForTx = true)
      sender.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee - minFee
      sender.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee - minFee
      sender.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L
      sender.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }


}
