package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.transaction.MassTransferTransactionData.Transfer
import com.wavesplatform.protobuf.transaction.{PBTransactions, Recipient}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.MaxTransferCount
import com.wavesplatform.transaction.transfer.TransferTransaction.MaxAttachmentSize
import io.grpc.Status.Code

class MassTransferTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("asset mass transfer changes asset balances and sender's.waves balance is decreased by fee.") {
    for (v <- massTransferTxSupportedVersions) {
      val firstBalance = miner.wavesBalance(firstAddress)
      val secondBalance = miner.wavesBalance(secondAddress)
      val attachment = ByteString.copyFrom("mass transfer description".getBytes("UTF-8"))

      val transfers = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount))
      val assetId = PBTransactions.vanilla(
        miner.broadcastIssue(firstAcc, "name", issueAmount, 8, reissuable = false, issueFee, waitForTx = true)
      ).explicitGet().id().toString
      miner.waitForTransaction(assetId)

      val massTransferTransactionFee = calcMassTransferFee(transfers.size)
      miner.broadcastMassTransfer(firstAcc, Some(assetId), transfers, attachment, massTransferTransactionFee, waitForTx = true)

      val firstBalanceAfter = miner.wavesBalance(firstAddress)
      val secondBalanceAfter = miner.wavesBalance(secondAddress)

      firstBalanceAfter.regular shouldBe firstBalance.regular - issueFee - massTransferTransactionFee
      firstBalanceAfter.effective shouldBe firstBalance.effective - issueFee - massTransferTransactionFee
      miner.assetsBalance(firstAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe issueAmount - transferAmount
      secondBalanceAfter.regular shouldBe secondBalance.regular
      secondBalanceAfter.effective shouldBe secondBalance.effective
      miner.assetsBalance(secondAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe transferAmount
    }
  }

  test("waves mass transfer changes waves balances") {
    val firstBalance = miner.wavesBalance(firstAddress)
    val secondBalance = miner.wavesBalance(secondAddress)
    val thirdBalance = miner.wavesBalance(thirdAddress)
    val transfers = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount), Transfer(Some(Recipient().withPublicKeyHash(thirdAddress)), 2 * transferAmount))

    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    miner.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee, waitForTx = true)

    val firstBalanceAfter = miner.wavesBalance(firstAddress)
    val secondBalanceAfter = miner.wavesBalance(secondAddress)
    val thirdBalanceAfter = miner.wavesBalance(thirdAddress)

    firstBalanceAfter.regular shouldBe firstBalance.regular - massTransferTransactionFee - 3 * transferAmount
    firstBalanceAfter.effective shouldBe firstBalance.effective - massTransferTransactionFee - 3 * transferAmount
    secondBalanceAfter.regular shouldBe secondBalance.regular + transferAmount
    secondBalanceAfter.effective shouldBe secondBalance.effective + transferAmount
    thirdBalanceAfter.regular shouldBe thirdBalance.regular + 2 * transferAmount
    thirdBalanceAfter.effective shouldBe thirdBalance.effective + 2 * transferAmount
  }

  test("can not make mass transfer without having enough waves") {
    val firstBalance = miner.wavesBalance(firstAddress)
    val secondBalance = miner.wavesBalance(secondAddress)
    val transfers        = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), firstBalance.regular / 2), Transfer(Some(Recipient().withPublicKeyHash(thirdAddress)), firstBalance.regular / 2))

    assertGrpcError(
      miner.broadcastMassTransfer(firstAcc, transfers = transfers, fee = calcMassTransferFee(transfers.size)),
      "Attempt to transfer unavailable funds",
      Code.INVALID_ARGUMENT
    )

    nodes.foreach(n => n.waitForHeight(n.height + 1))
    miner.wavesBalance(firstAddress) shouldBe firstBalance
    miner.wavesBalance(secondAddress) shouldBe secondBalance
  }

  test("cannot make mass transfer when fee less then minimal ") {
    val firstBalance = miner.wavesBalance(firstAddress)
    val secondBalance = miner.wavesBalance(secondAddress)
    val transfers        = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)

    assertGrpcError(
      miner.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee - 1),
      s"does not exceed minimal value of $massTransferTransactionFee WAVES",
      Code.INVALID_ARGUMENT
    )

    nodes.foreach(n => n.waitForHeight(n.height + 1))
    miner.wavesBalance(firstAddress) shouldBe firstBalance
    miner.wavesBalance(secondAddress) shouldBe secondBalance
  }

  test("cannot make mass transfer without having enough of effective balance") {
    val firstBalance = miner.wavesBalance(firstAddress)
    val secondBalance = miner.wavesBalance(secondAddress)
    val transfers        = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), firstBalance.regular - 2 * minFee))
    val massTransferTransactionFee = calcMassTransferFee(transfers.size)

    miner.broadcastLease(firstAcc, Recipient().withPublicKeyHash(secondAddress), leasingAmount, minFee, waitForTx = true)

    assertGrpcError(
      miner.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee),
      "Attempt to transfer unavailable funds",
      Code.INVALID_ARGUMENT
    )
    nodes.foreach(n => n.waitForHeight(n.height + 1))
    miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
    miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
    miner.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
    miner.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount
  }

  test("cannot broadcast invalid mass transfer tx") {
    val firstBalance = miner.wavesBalance(firstAddress)
    val secondBalance = miner.wavesBalance(secondAddress)
    val defaultTransfer = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), transferAmount))

    val negativeTransfer = List(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), -1))
    assertGrpcError(
      miner.broadcastMassTransfer(firstAcc, transfers = negativeTransfer, fee = calcMassTransferFee(negativeTransfer.size)),
      "One of the transfers has negative amount",
      Code.INVALID_ARGUMENT
    )

    val tooManyTransfers = List.fill(MaxTransferCount + 1)(Transfer(Some(Recipient().withPublicKeyHash(secondAddress)), 1))
    assertGrpcError(
      miner.broadcastMassTransfer(firstAcc, transfers = tooManyTransfers, fee = calcMassTransferFee(MaxTransferCount + 1)),
      s"Number of transfers ${MaxTransferCount + 1} is greater than 100",
      Code.INVALID_ARGUMENT
    )

    val tooBigAttachment = ByteString.copyFrom(("a" * (MaxAttachmentSize + 1)).getBytes("UTF-8"))
    assertGrpcError(
      miner.broadcastMassTransfer(firstAcc, transfers = defaultTransfer, attachment = tooBigAttachment, fee = calcMassTransferFee(1)),
      "Too big sequence requested",
      Code.INVALID_ARGUMENT
    )

    miner.wavesBalance(firstAddress) shouldBe firstBalance
    miner.wavesBalance(secondAddress) shouldBe secondBalance
  }

  test("huge transactions are allowed") {
    val firstBalance = miner.wavesBalance(firstAddress)
    val fee = calcMassTransferFee(MaxTransferCount)
    val amount = (firstBalance.available - fee) / MaxTransferCount
    val maxAttachment = ByteString.copyFrom(("a" * MaxAttachmentSize).getBytes("UTF-8"))


    val transfers  = List.fill(MaxTransferCount)(Transfer(Some(Recipient().withPublicKeyHash(firstAddress)), amount))
    miner.broadcastMassTransfer(firstAcc, transfers = transfers, fee = fee, attachment = maxAttachment, waitForTx = true)

    miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - fee
    miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - fee
  }

  test("able to mass transfer to alias") {
    val firstBalance = miner.wavesBalance(firstAddress)
    val secondBalance = miner.wavesBalance(secondAddress)

    val alias = "masstest_alias"

    miner.broadcastCreateAlias(secondAcc, alias, minFee, waitForTx = true)

    val transfers = List(Transfer(Some(Recipient().withPublicKeyHash(firstAddress)), transferAmount), Transfer(Some(Recipient().withAlias(alias)), transferAmount))

    val massTransferTransactionFee = calcMassTransferFee(transfers.size)
    miner.broadcastMassTransfer(firstAcc, transfers = transfers, fee = massTransferTransactionFee, waitForTx = true)

    miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - massTransferTransactionFee - transferAmount
    miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - massTransferTransactionFee - transferAmount
    miner.wavesBalance(secondAddress).regular shouldBe secondBalance.regular + transferAmount - minFee
    miner.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + transferAmount - minFee
  }
}
