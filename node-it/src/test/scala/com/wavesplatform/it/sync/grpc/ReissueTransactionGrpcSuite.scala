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
    for (v <- reissueTxSupportedVersions) {
      val reissuerBalance = miner.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = miner.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = miner.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      miner.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true)

      miner.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee
      miner.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee
      miner.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }

  test("can't reissue not reissuable asset") {
    for (v <- reissueTxSupportedVersions) {
      val reissuerBalance = miner.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = miner.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = miner.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = false, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      assertGrpcError(miner.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, version = v, reissuable = true, waitForTx = true),
        "Asset is not reissuable",
        Code.INVALID_ARGUMENT)

      miner.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee
      miner.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee
      miner.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("not able to reissue if cannot pay fee - insufficient funds") {
    for (v <- reissueTxSupportedVersions) {
      val reissuerBalance = miner.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = miner.wavesBalance(reissuerAddress).effective
      val hugeReissueFee = reissuerEffBalance + 1.waves

      val issuedAssetTx = miner.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      assertGrpcError(miner.broadcastReissue(reissuer, hugeReissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT)

      miner.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee
      miner.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee
      miner.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("asset becomes non-reissuable after reissue with reissuable=false") {
    for (v <- reissueTxSupportedVersions) {
      val reissuerBalance = miner.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = miner.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = miner.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      miner.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = false, version = v, waitForTx = true)

      assertGrpcError(miner.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true),
        "Asset is not reissuable",
        Code.INVALID_ARGUMENT)

      miner.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee
      miner.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee
      miner.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }

  test("able to transfer new reissued amount of assets") {
    for (v <- reissueTxSupportedVersions) {
      val reissuerBalance = miner.wavesBalance(reissuerAddress).available
      val reissuerEffBalance = miner.wavesBalance(reissuerAddress).effective

      val issuedAssetTx = miner.broadcastIssue(reissuer, "assetname", someAssetAmount, decimals = 2, reissuable = true, issueFee, waitForTx = true)
      val issuedAssetId = PBTransactions.vanilla(issuedAssetTx).explicitGet().id().toString

      miner.broadcastReissue(reissuer, reissueFee, issuedAssetId, someAssetAmount, reissuable = true, version = v, waitForTx = true)

      miner.broadcastTransfer(reissuer, Recipient().withPublicKeyHash(secondAddress), 2 * someAssetAmount, minFee, assetId = issuedAssetId, waitForTx = true)
      miner.wavesBalance(reissuerAddress).available shouldBe reissuerBalance - issueFee - reissueFee - minFee
      miner.wavesBalance(reissuerAddress).effective shouldBe reissuerEffBalance - issueFee - reissueFee - minFee
      miner.assetsBalance(reissuerAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L
      miner.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 2 * someAssetAmount
    }
  }


}
