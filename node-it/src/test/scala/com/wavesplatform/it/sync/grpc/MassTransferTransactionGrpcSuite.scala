package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.Alias
import com.wavesplatform.api.http.assets.{MassTransferRequest, SignedMassTransferRequest}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.MassTransferTransaction.MaxTransferCount
import com.wavesplatform.transaction.transfer.TransferTransaction.MaxAttachmentSize
import com.wavesplatform.protobuf.transaction.MassTransferTransactionData.Transfer
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.common.utils.EitherExt2
import io.grpc.Status.Code

import scala.util.Random

class MassTransferTransactionGrpcSuite extends GrpcBaseTransactionSuite {
  private def fakeSignature = Base58.encode(Array.fill(64)(Random.nextInt.toByte))

  test("asset mass transfer changes asset balances and sender's.waves balance is decreased by fee.") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val attachment = ByteString.copyFrom("mass transfer description".getBytes("UTF-8"))

    val transfers = List(Transfer(Some(Recipient().withAddress(secondAddress)), transferAmount))
    val assetId   = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(firstAcc, "name", issueAmount, 8, reissuable = false, issueFee, waitForTx = true)
    ).explicitGet().id().base58
    nodes.waitForHeightAriseAndTxPresent(assetId)

    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    sender.grpc.broadcastMassTransfer(firstAcc, Some(assetId), transfers, attachment, massTransferTransactionFee, waitForTx = true)

    val firstBalanceAfter = sender.grpc.wavesBalance(firstAddress)
    val secondBalanceAfter = sender.grpc.wavesBalance(secondAddress)

    firstBalanceAfter.regular shouldBe firstBalance.regular - issueFee - massTransferTransactionFee
    firstBalanceAfter.effective shouldBe firstBalance.effective - issueFee - massTransferTransactionFee
    sender.grpc.assetsBalance(firstAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe issueAmount - transferAmount
    secondBalanceAfter.regular shouldBe secondBalance.regular
    secondBalanceAfter.effective shouldBe secondBalance.effective
    sender.grpc.assetsBalance(secondAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe transferAmount
  }

  test("waves mass transfer changes waves balances") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val thirdBalance = sender.grpc.wavesBalance(thirdAddress)
    val transfers = List(Transfer(Some(Recipient().withAddress(secondAddress)), transferAmount), Transfer(Some(Recipient().withAddress(thirdAddress)), 2 * transferAmount))

    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    sender.grpc.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee, waitForTx = true)

    val firstBalanceAfter = sender.grpc.wavesBalance(firstAddress)
    val secondBalanceAfter = sender.grpc.wavesBalance(secondAddress)
    val thirdBalanceAfter = sender.grpc.wavesBalance(thirdAddress)

    firstBalanceAfter.regular shouldBe firstBalance.regular - massTransferTransactionFee - 3 * transferAmount
    firstBalanceAfter.effective shouldBe firstBalance.effective - massTransferTransactionFee - 3 * transferAmount
    secondBalanceAfter.regular shouldBe secondBalance.regular + transferAmount
    secondBalanceAfter.effective shouldBe secondBalance.effective + transferAmount
    thirdBalanceAfter.regular shouldBe thirdBalance.regular + 2 * transferAmount
    thirdBalanceAfter.effective shouldBe thirdBalance.effective + 2 * transferAmount
  }

  test("can not make mass transfer without having enough waves") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val transfers        = List(Transfer(Some(Recipient().withAddress(secondAddress)), firstBalance.regular / 2), Transfer(Some(Recipient().withAddress(thirdAddress)), firstBalance.regular / 2))

    assertGrpcError(
      sender.grpc.broadcastMassTransfer(firstAcc, transfers = transfers, fee = calcMassTransferFee(transfers.size)),
      "negative waves balance",
      Code.INVALID_ARGUMENT
    )

    nodes.foreach(n => n.grpc.waitForHeight(n.grpc.height + 1))
    sender.grpc.wavesBalance(firstAddress) shouldBe firstBalance
    sender.grpc.wavesBalance(secondAddress) shouldBe secondBalance
  }

  test("can not make mass transfer when fee less then mininal ") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val transfers        = List(Transfer(Some(Recipient().withAddress(secondAddress)), transferAmount))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)

    assertGrpcError(
      sender.grpc.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee - 1),
      s"does not exceed minimal value of $massTransferTransactionFee WAVES",
      Code.INVALID_ARGUMENT
    )

    nodes.foreach(n => n.grpc.waitForHeight(n.grpc.height + 1))
    sender.grpc.wavesBalance(firstAddress) shouldBe firstBalance
    sender.grpc.wavesBalance(secondAddress) shouldBe secondBalance
  }

}
