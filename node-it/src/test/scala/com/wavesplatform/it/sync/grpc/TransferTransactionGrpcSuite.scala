package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import io.grpc.Status.Code

import scala.concurrent.duration._

class TransferTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime {

  var issuedAssetId: String = _
  override def beforeAll(): Unit = {
    super.beforeAll()

    val issuedAsset = miner.broadcastIssue(firstAcc, "name", someAssetAmount, 8, true, issueFee, waitForTx = true)
    issuedAssetId = PBTransactions.vanilla(issuedAsset).explicitGet().id().toString
  }

  test("asset transfer changes sender's and recipient's asset balance by transfer amount and waves by fee") {
    for (v <- transferTxSupportedVersions) {
      val issuedAsset      = miner.broadcastIssue(firstAcc, "name", someAssetAmount, 8, true, issueFee, waitForTx = true)
      val issuedAssetId    = PBTransactions.vanilla(issuedAsset).explicitGet().id().toString
      val firstBalance     = miner.wavesBalance(firstAddress).available
      val firstEffBalance  = miner.wavesBalance(firstAddress).effective
      val secondBalance    = miner.wavesBalance(secondAddress).available
      val secondEffBalance = miner.wavesBalance(secondAddress).effective

      miner.broadcastTransfer(
        firstAcc,
        Recipient().withPublicKeyHash(secondAddress),
        someAssetAmount,
        minFee,
        version = v,
        issuedAssetId,
        waitForTx = true
      )

      miner.wavesBalance(firstAddress).available shouldBe firstBalance - minFee
      miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance - minFee
      miner.wavesBalance(secondAddress).available shouldBe secondBalance
      miner.wavesBalance(secondAddress).effective shouldBe secondEffBalance

      miner.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0
      miner.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("waves transfer changes waves balances and eff.b. by transfer amount and fee") {
    for (v <- transferTxSupportedVersions) {
      val firstBalance     = miner.wavesBalance(firstAddress).available
      val firstEffBalance  = miner.wavesBalance(firstAddress).effective
      val secondBalance    = miner.wavesBalance(secondAddress).available
      val secondEffBalance = miner.wavesBalance(secondAddress).effective

      miner.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferAmount, minFee, version = v, waitForTx = true)

      miner.wavesBalance(firstAddress).available shouldBe firstBalance - transferAmount - minFee
      miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance - transferAmount - minFee
      miner.wavesBalance(secondAddress).available shouldBe secondBalance + transferAmount
      miner.wavesBalance(secondAddress).effective shouldBe secondEffBalance + transferAmount
    }
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    val invalidTimestampFromFuture = ntpTime.correctedTime() + 91.minutes.toMillis
    val invalidTimestampFromPast   = ntpTime.correctedTime() - 121.minutes.toMillis
    for (v <- transferTxSupportedVersions) {
      val firstBalance     = miner.wavesBalance(firstAddress).available
      val firstEffBalance  = miner.wavesBalance(firstAddress).effective
      val secondBalance    = miner.wavesBalance(secondAddress).available
      val secondEffBalance = miner.wavesBalance(secondAddress).effective

      assertGrpcError(
        miner.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
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
        miner.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
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
        miner.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferAmount, minFee - 1, version = v, waitForTx = true),
        "Fee .* does not exceed minimal value",
        Code.INVALID_ARGUMENT
      )

      miner.wavesBalance(firstAddress).available shouldBe firstBalance
      miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance
      miner.wavesBalance(secondAddress).available shouldBe secondBalance
      miner.wavesBalance(secondAddress).effective shouldBe secondEffBalance
    }
  }

  test("can not make transfer without having enough waves balance") {
    for (v <- transferTxSupportedVersions) {
      val firstBalance     = miner.wavesBalance(firstAddress).available
      val firstEffBalance  = miner.wavesBalance(firstAddress).effective
      val secondBalance    = miner.wavesBalance(secondAddress).available
      val secondEffBalance = miner.wavesBalance(secondAddress).effective

      assertGrpcError(
        miner.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), firstBalance, minFee, v, waitForTx = true),
        "Attempt to transfer unavailable funds",
        Code.INVALID_ARGUMENT
      )

      miner.wavesBalance(firstAddress).available shouldBe firstBalance
      miner.wavesBalance(firstAddress).effective shouldBe firstEffBalance
      miner.wavesBalance(secondAddress).available shouldBe secondBalance
      miner.wavesBalance(secondAddress).effective shouldBe secondEffBalance
    }
  }

  test("can not make assets transfer without having enough assets balance") {
    for (v <- transferTxSupportedVersions) {
      val firstAssetBalance  = miner.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L)
      val secondAssetBalance = miner.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L)

      assertGrpcError(
        miner.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
          firstAssetBalance + 1,
          minFee,
          version = v,
          assetId = issuedAssetId,
          waitForTx = true
        ),
        "Attempt to transfer unavailable funds",
        Code.INVALID_ARGUMENT
      )

      miner.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe firstAssetBalance
      miner.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe secondAssetBalance
    }
  }
}
