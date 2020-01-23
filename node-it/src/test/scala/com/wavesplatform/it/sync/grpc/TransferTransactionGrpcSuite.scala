package com.wavesplatform.it.sync.grpc

import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.Recipient
import io.grpc.Status.Code

import scala.concurrent.duration._

class TransferTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime {

  var issuedAssetId: String = _
  override def beforeAll(): Unit = {
    super.beforeAll()

    val issuedAsset = sender.broadcastIssue(firstAcc, "name", someAssetAmount, 8, true, issueFee, waitForTx = true)
    issuedAssetId = PBTransactions.vanilla(issuedAsset).explicitGet().id().base58
  }

  test("asset transfer changes sender's and recipient's asset balance by transfer amount and waves by fee") {
    for (v <- supportedVersions) {
      val issuedAsset      = sender.broadcastIssue(firstAcc, "name", someAssetAmount, 8, true, issueFee, waitForTx = true)
      val issuedAssetId    = PBTransactions.vanilla(issuedAsset).explicitGet().id().base58
      val firstBalance     = sender.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.wavesBalance(firstAddress).effective
      val secondBalance    = sender.wavesBalance(secondAddress).available
      val secondEffBalance = sender.wavesBalance(secondAddress).effective

      sender.broadcastTransfer(
        firstAcc,
        Recipient().withAddress(secondAddress),
        someAssetAmount,
        minFee,
        version = v,
        issuedAssetId,
        waitForTx = true
      )

      sender.wavesBalance(firstAddress).available shouldBe firstBalance - minFee
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance - minFee
      sender.wavesBalance(secondAddress).available shouldBe secondBalance
      sender.wavesBalance(secondAddress).effective shouldBe secondEffBalance

      sender.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0
      sender.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("waves transfer changes waves balances and eff.b. by transfer amount and fee") {
    for (v <- supportedVersions) {
      val firstBalance     = sender.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.wavesBalance(firstAddress).effective
      val secondBalance    = sender.wavesBalance(secondAddress).available
      val secondEffBalance = sender.wavesBalance(secondAddress).effective

      sender.broadcastTransfer(firstAcc, Recipient().withAddress(secondAddress), transferAmount, minFee, version = v, waitForTx = true)

      sender.wavesBalance(firstAddress).available shouldBe firstBalance - transferAmount - minFee
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance - transferAmount - minFee
      sender.wavesBalance(secondAddress).available shouldBe secondBalance + transferAmount
      sender.wavesBalance(secondAddress).effective shouldBe secondEffBalance + transferAmount
    }
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    val invalidTimestampFromFuture = ntpTime.correctedTime() + 91.minutes.toMillis
    val invalidTimestampFromPast   = ntpTime.correctedTime() - 121.minutes.toMillis
    for (v <- supportedVersions) {
      val firstBalance     = sender.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.wavesBalance(firstAddress).effective
      val secondBalance    = sender.wavesBalance(secondAddress).available
      val secondEffBalance = sender.wavesBalance(secondAddress).effective

      assertGrpcError(
        sender.broadcastTransfer(
          firstAcc,
          Recipient().withAddress(secondAddress),
          transferAmount,
          minFee,
          timestamp = invalidTimestampFromFuture,
          version = v,
          waitForTx = true
        ),
        "Transaction timestamp .* is more than .*ms in the future",
        Code.INVALID_ARGUMENT
      )
      assertGrpcError(
        sender.broadcastTransfer(
          firstAcc,
          Recipient().withAddress(secondAddress),
          transferAmount,
          minFee,
          timestamp = invalidTimestampFromPast,
          version = v,
          waitForTx = true
        ),
        "Transaction timestamp .* is more than .*ms in the past",
        Code.INVALID_ARGUMENT
      )
      assertGrpcError(
        sender.broadcastTransfer(firstAcc, Recipient().withAddress(secondAddress), transferAmount, minFee - 1, version = v, waitForTx = true),
        "Fee .* does not exceed minimal value",
        Code.INVALID_ARGUMENT
      )

      sender.wavesBalance(firstAddress).available shouldBe firstBalance
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
      sender.wavesBalance(secondAddress).available shouldBe secondBalance
      sender.wavesBalance(secondAddress).effective shouldBe secondEffBalance
    }
  }

  test("can not make transfer without having enough waves balance") {
    for (v <- supportedVersions) {
      val firstBalance     = sender.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.wavesBalance(firstAddress).effective
      val secondBalance    = sender.wavesBalance(secondAddress).available
      val secondEffBalance = sender.wavesBalance(secondAddress).effective

      assertGrpcError(
        sender.broadcastTransfer(firstAcc, Recipient().withAddress(secondAddress), firstBalance, minFee, v, waitForTx = true),
        "Attempt to transfer unavailable funds",
        Code.INVALID_ARGUMENT
      )

      sender.wavesBalance(firstAddress).available shouldBe firstBalance
      sender.wavesBalance(firstAddress).effective shouldBe firstEffBalance
      sender.wavesBalance(secondAddress).available shouldBe secondBalance
      sender.wavesBalance(secondAddress).effective shouldBe secondEffBalance
    }
  }

  test("can not make assets transfer without having enough assets balance") {
    for (v <- supportedVersions) {
      val firstAssetBalance  = sender.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L)
      val secondAssetBalance = sender.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L)

      assertGrpcError(
        sender.broadcastTransfer(
          firstAcc,
          Recipient().withAddress(secondAddress),
          firstAssetBalance + 1,
          minFee,
          version = v,
          assetId = issuedAssetId,
          waitForTx = true
        ),
        "Attempt to transfer unavailable funds",
        Code.INVALID_ARGUMENT
      )

      sender.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe firstAssetBalance
      sender.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe secondAssetBalance
    }
  }

}
