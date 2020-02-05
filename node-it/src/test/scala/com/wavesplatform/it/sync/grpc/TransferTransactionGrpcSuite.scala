package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.transaction.Attachment.Attachment
import com.wavesplatform.protobuf.transaction.Attachment.Attachment.{BinaryValue, BoolValue, IntValue, StringValue}
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import io.grpc.Status.Code

import scala.concurrent.duration._

class TransferTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime {

  var issuedAssetId: String = _
  override def beforeAll(): Unit = {
    super.beforeAll()

    val issuedAsset = sender.grpc.broadcastIssue(firstAcc, "name", someAssetAmount, 8, true, issueFee, waitForTx = true)
    issuedAssetId = PBTransactions.vanilla(issuedAsset).explicitGet().id().toString
  }

  test("asset transfer changes sender's and recipient's asset balance by transfer amount and waves by fee") {
    for (v <- transferTxSupportedVersions) {
      val issuedAsset      = sender.grpc.broadcastIssue(firstAcc, "name", someAssetAmount, 8, true, issueFee, waitForTx = true)
      val issuedAssetId    = PBTransactions.vanilla(issuedAsset).explicitGet().id().toString
      val firstBalance     = sender.grpc.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.grpc.wavesBalance(firstAddress).effective
      val secondBalance    = sender.grpc.wavesBalance(secondAddress).available
      val secondEffBalance = sender.grpc.wavesBalance(secondAddress).effective

      sender.grpc.broadcastTransfer(
        firstAcc,
        Recipient().withPublicKeyHash(secondAddress),
        someAssetAmount,
        minFee,
        version = v,
        issuedAssetId,
        waitForTx = true
      )

      sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance - minFee
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance - minFee
      sender.grpc.wavesBalance(secondAddress).available shouldBe secondBalance
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondEffBalance

      sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe 0
      sender.grpc.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe someAssetAmount
    }
  }

  test("waves transfer changes waves balances and eff.b. by transfer amount and fee") {
    for (v <- transferTxSupportedVersions) {
      val firstBalance     = sender.grpc.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.grpc.wavesBalance(firstAddress).effective
      val secondBalance    = sender.grpc.wavesBalance(secondAddress).available
      val secondEffBalance = sender.grpc.wavesBalance(secondAddress).effective

      sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferAmount, minFee, version = v, waitForTx = true)

      sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance - transferAmount - minFee
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance - transferAmount - minFee
      sender.grpc.wavesBalance(secondAddress).available shouldBe secondBalance + transferAmount
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondEffBalance + transferAmount
    }
  }

  test("invalid signed waves transfer should not be in UTX or blockchain") {
    val invalidTimestampFromFuture = ntpTime.correctedTime() + 91.minutes.toMillis
    val invalidTimestampFromPast   = ntpTime.correctedTime() - 121.minutes.toMillis
    for (v <- transferTxSupportedVersions) {
      val firstBalance     = sender.grpc.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.grpc.wavesBalance(firstAddress).effective
      val secondBalance    = sender.grpc.wavesBalance(secondAddress).available
      val secondEffBalance = sender.grpc.wavesBalance(secondAddress).effective

      assertGrpcError(
        sender.grpc.broadcastTransfer(
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
        sender.grpc.broadcastTransfer(
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
        sender.grpc
          .broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferAmount, minFee - 1, version = v, waitForTx = true),
        "Fee .* does not exceed minimal value",
        Code.INVALID_ARGUMENT
      )

      sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance
      sender.grpc.wavesBalance(secondAddress).available shouldBe secondBalance
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondEffBalance
    }
  }

  test("can not make transfer without having enough waves balance") {
    for (v <- transferTxSupportedVersions) {
      val firstBalance     = sender.grpc.wavesBalance(firstAddress).available
      val firstEffBalance  = sender.grpc.wavesBalance(firstAddress).effective
      val secondBalance    = sender.grpc.wavesBalance(secondAddress).available
      val secondEffBalance = sender.grpc.wavesBalance(secondAddress).effective

      assertGrpcError(
        sender.grpc.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), firstBalance, minFee, v, waitForTx = true),
        "Attempt to transfer unavailable funds",
        Code.INVALID_ARGUMENT
      )

      sender.grpc.wavesBalance(firstAddress).available shouldBe firstBalance
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstEffBalance
      sender.grpc.wavesBalance(secondAddress).available shouldBe secondBalance
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondEffBalance
    }
  }

  test("can not make assets transfer without having enough assets balance") {
    for (v <- transferTxSupportedVersions) {
      val firstAssetBalance  = sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L)
      val secondAssetBalance = sender.grpc.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L)

      assertGrpcError(
        sender.grpc.broadcastTransfer(
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

      sender.grpc.assetsBalance(firstAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe firstAssetBalance
      sender.grpc.assetsBalance(secondAddress, Seq(issuedAssetId)).getOrElse(issuedAssetId, 0L) shouldBe secondAssetBalance
    }
  }
  test("able to pass typed attachment to transfer transaction V3") {
    val txWithStringAtt = PBTransactions
      .vanilla(
        sender.grpc.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
          transferAmount,
          minFee,
          version = 3,
          attachment = Attachment.StringValue("somestring"),
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString

    val txWithBoolAtt = PBTransactions
      .vanilla(
        sender.grpc.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
          transferAmount,
          minFee,
          version = 3,
          attachment = Attachment.BoolValue(false),
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString

    val txWithIntAtt = PBTransactions
      .vanilla(
        sender.grpc.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
          transferAmount,
          minFee,
          version = 3,
          attachment = Attachment.IntValue(123),
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString

    val txWithBinaryAtt = PBTransactions
      .vanilla(
        sender.grpc.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
          transferAmount,
          minFee,
          version = 3,
          attachment = Attachment.BinaryValue(firstAddress),
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString

    sender.grpc.getTransaction(txWithStringAtt).getTransaction.getTransfer.getAttachment.getStringValue shouldBe "somestring"
    sender.grpc.getTransaction(txWithBoolAtt).getTransaction.getTransfer.getAttachment.getBoolValue shouldBe false
    sender.grpc.getTransaction(txWithIntAtt).getTransaction.getTransfer.getAttachment.getIntValue shouldBe 123
    sender.grpc.getTransaction(txWithBinaryAtt).getTransaction.getTransfer.getAttachment.getBinaryValue shouldBe firstAddress
  }

  test("not able to pass typed attachment for transactions V < 3") {
    for (v <- transferTxSupportedVersions if v < 3) {
      assertGrpcError(
        sender.grpc.broadcastTransfer(
          firstAcc,
          Recipient().withPublicKeyHash(secondAddress),
          transferAmount,
          minFee,
          version = v,
          attachment = Attachment.StringValue("somestring"),
          waitForTx = true
        ),
        "Str\\(somestring\\) can not be strictly converted to bytes",
        Code.INTERNAL
      )
    }
  }
}
