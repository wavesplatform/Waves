package com.wavesplatform.it.sync.grpc

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.common.utils.EitherExt2
import io.grpc.Status.Code

class BurnTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  private val decimals: Byte = 2

  test("burning assets changes issuer's asset balance; issuer's waves balance is decreased by fee") {
    for (v <- supportedVersions) {
      val issuedAssetId = PBTransactions.vanilla(
        sender.grpc.broadcastIssue(firstAcc, s"name+$v", issueAmount, decimals, reissuable = false, fee = issueFee, waitForTx = true)
      ).explicitGet().id().toString

      sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId))(issuedAssetId) shouldBe issueAmount

      // burn half of the coins and check balance
      val balance = sender.grpc.wavesBalance(firstAddress)
      sender.grpc.broadcastBurn(firstAcc, issuedAssetId, issueAmount / 2, minFee, version = v, waitForTx = true)

      sender.grpc.wavesBalance(firstAddress).available shouldBe balance.available - minFee
      sender.grpc.wavesBalance(firstAddress).effective shouldBe balance.effective - minFee

      sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId))(issuedAssetId) shouldBe issueAmount / 2

      // burn the rest and check again
      sender.grpc.broadcastBurn(firstAcc, issuedAssetId, issueAmount / 2, minFee, version = v, waitForTx = true)
      sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L
    }
  }

  test("non-issuer able to burn assets that he own") {
    for (v <- supportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2

      val issuedAssetId = PBTransactions.vanilla(
        sender.grpc.broadcastIssue(firstAcc, s"name+$v", issuedQuantity, decimals, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString
      sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferredQuantity, minFee, assetId = issuedAssetId, waitForTx = true)

      sender.grpc.broadcastBurn(secondAcc, issuedAssetId, transferredQuantity, minFee, version = v, waitForTx = true)
      sender.grpc.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L

      assertGrpcError(
        sender.grpc.broadcastTransfer(secondAcc, Recipient().withPublicKeyHash(firstAddress), transferredQuantity, minFee, assetId = issuedAssetId),
        "Attempt to transfer unavailable funds",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("issuer can't burn more tokens than he own") {
    for (v <- supportedVersions) {
      val issuedQuantity = issueAmount
      val burnedQuantity = issuedQuantity + 1

      val issuedAssetId = PBTransactions.vanilla(
        sender.grpc.broadcastIssue(firstAcc, s"name+$v", issuedQuantity, decimals, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString

      assertGrpcError(
        sender.grpc.broadcastBurn(firstAcc, issuedAssetId, burnedQuantity, minFee, version = v),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("user can't burn more tokens than he own") {
    for (v <- supportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2
      val burnedQuantity      = transferredQuantity + 1

      val issuedAssetId = PBTransactions.vanilla(
        sender.grpc.broadcastIssue(firstAcc, s"name+$v", issuedQuantity, decimals, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString
      sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferredQuantity, minFee, assetId = issuedAssetId, waitForTx = true)

      assertGrpcError(
        sender.grpc.broadcastBurn(secondAcc, issuedAssetId, burnedQuantity, minFee, version = v),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test("non-owner can burn asset after reissue") {
    for (v <- supportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2

      val issuedAssetId = PBTransactions.vanilla(
        sender.grpc.broadcastIssue(firstAcc, s"name+$v", issuedQuantity, decimals, reissuable = true, issueFee, waitForTx = true)
      ).explicitGet().id().toString

      sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferredQuantity, minFee, assetId = issuedAssetId, waitForTx = true)
      sender.grpc.broadcastBurn(firstAcc, issuedAssetId, transferredQuantity, minFee, v, waitForTx = true)

      sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L
      sender.grpc.assetsBalance(secondAddress, Seq(issuedAssetId))(issuedAssetId) shouldBe transferredQuantity

      sender.grpc.broadcastReissue(firstAcc, issueFee, issuedAssetId, issuedQuantity, waitForTx = true)

      sender.grpc.broadcastBurn(firstAcc, issuedAssetId, issuedQuantity, minFee, v, waitForTx = true)
      sender.grpc.broadcastBurn(secondAcc, issuedAssetId, transferredQuantity, minFee, v, waitForTx = true)

      sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L
      sender.grpc.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0L
    }
  }

}
