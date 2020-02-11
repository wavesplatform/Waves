package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.transaction.transfer.MassTransferTransaction.MaxTransferCount
import com.wavesplatform.transaction.transfer.TransferTransaction.MaxAttachmentSize
import com.wavesplatform.protobuf.transaction.MassTransferTransactionData.Transfer
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.common.utils.EitherExt2
import io.grpc.Status.Code
import org.scalatest.Ignore

class MassTransferTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("asset mass transfer changes asset balances and sender's.waves balance is decreased by fee.") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val attachment = ByteString.copyFrom("mass transfer description".getBytes("UTF-8"))

    val transfers = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount))
    val assetId   = PBTransactions.vanilla(
      sender.grpc.broadcastIssue(firstAcc, "name", issueAmount, 8, reissuable = false, issueFee, waitForTx = true)
    ).explicitGet().id().toString
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
    val transfers = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount), Transfer(Some(Recipient().withPublicKeyHash(thirdAddress)), 2 * transferAmount))

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
    val transfers        = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), firstBalance.regular / 2), Transfer(Some(Recipient().withPublicKeyHash(thirdAddress)), firstBalance.regular / 2))

    assertGrpcError(
      sender.grpc.broadcastMassTransfer(firstAcc, transfers = transfers, fee = calcMassTransferFee(transfers.size)),
      "Attempt to transfer unavailable funds",
      Code.INVALID_ARGUMENT
    )

    nodes.foreach(n => n.grpc.waitForHeight(n.grpc.height + 1))
    sender.grpc.wavesBalance(firstAddress) shouldBe firstBalance
    sender.grpc.wavesBalance(secondAddress) shouldBe secondBalance
  }

  test("cannot make mass transfer when fee less then minimal ") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val transfers        = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount))
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

  test("cannot make mass transfer without having enough of effective balance") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val transfers        = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), firstBalance.regular - 2 * minFee))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)

    sender.grpc.broadcastLease(firstAcc, Recipient().withPublicKeyHash(secondAddress), leasingAmount, minFee, waitForTx = true)

    assertGrpcError(
      sender.grpc.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee),
      "Attempt to transfer unavailable funds",
      Code.INVALID_ARGUMENT
    )
    nodes.foreach(n => n.grpc.waitForHeight(n.grpc.height + 1))
    sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
    sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
    sender.grpc.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
    sender.grpc.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount
  }

  test("cannot broadcast invalid mass transfer tx") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)
    val defaultTransfer = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount))

    val negativeTransfer = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), -1))
    assertGrpcError(
      sender.grpc.broadcastMassTransfer(firstAcc, transfers = negativeTransfer, fee = calcMassTransferFee(negativeTransfer.size)),
      "One of the transfers has negative amount",
      Code.INVALID_ARGUMENT
    )

    val tooManyTransfers = List.fill(MaxTransferCount + 1)(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), 1))
    assertGrpcError(
      sender.grpc.broadcastMassTransfer(firstAcc, transfers = tooManyTransfers, fee = calcMassTransferFee(MaxTransferCount + 1)),
      s"Number of transfers ${MaxTransferCount + 1} is greater than 100",
      Code.INVALID_ARGUMENT
    )

    val tooBigAttachment = ByteString.copyFrom(("a" * (MaxAttachmentSize + 1)).getBytes("UTF-8"))
    assertGrpcError(
      sender.grpc.broadcastMassTransfer(firstAcc, transfers = defaultTransfer, attachment = tooBigAttachment, fee = calcMassTransferFee(1)),
      "Too big sequences requested",
      Code.INVALID_ARGUMENT
    )

    sender.grpc.wavesBalance(firstAddress) shouldBe firstBalance
    sender.grpc.wavesBalance(secondAddress) shouldBe secondBalance
  }

  test("huge transactions are allowed") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val fee = calcMassTransferFee(MaxTransferCount)
    val amount = (firstBalance.available - fee) / MaxTransferCount
    val maxAttachment = ByteString.copyFrom(("a" * MaxAttachmentSize).getBytes("UTF-8"))


    val transfers  = List.fill(MaxTransferCount)(Transfer(Some(Recipient().withPublicKeyHash(firstAddress)), amount))
    sender.grpc.broadcastMassTransfer(firstAcc, transfers = transfers, fee = fee, attachment = maxAttachment, waitForTx = true)

    sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - fee
    sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - fee
  }

  test("able to mass transfer to alias") {
    val firstBalance = sender.grpc.wavesBalance(firstAddress)
    val secondBalance = sender.grpc.wavesBalance(secondAddress)

    val alias = "masstest_alias"

    sender.grpc.broadcastCreateAlias(secondAcc, alias, minFee, waitForTx = true)

    val transfers = List(Transfer(Some(Recipient().withPublicKeyHash(firstAddress)), transferAmount), Transfer(Some(Recipient().withAlias(alias)), transferAmount))

    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    sender.grpc.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee, waitForTx = true)

    sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - massTransferTransactionFee - transferAmount
    sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - massTransferTransactionFee - transferAmount
    sender.grpc.wavesBalance(secondAddress).regular shouldBe secondBalance.regular + transferAmount - minFee
    sender.grpc.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + transferAmount - minFee
  }

}
