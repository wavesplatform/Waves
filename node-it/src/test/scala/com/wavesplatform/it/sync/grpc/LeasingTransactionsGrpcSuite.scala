package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.api.grpc.LeaseResponse
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions, Recipient}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import io.grpc.Status.Code

class LeasingTransactionsGrpcSuite extends GrpcBaseTransactionSuite {
  private val errorMessage = "Reason: Cannot lease more than own"

  test("leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    for (v <- leaseTxSupportedVersions) {
      val firstBalance  = miner.wavesBalance(firstAddress)
      val secondBalance = miner.wavesBalance(secondAddress)

      val leaseTx   = miner.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val vanillaTx = PBTransactions.vanilla(leaseTx).explicitGet()
      val leaseTxId = vanillaTx.id().toString
      val height    = sender.getStatus(leaseTxId).height

      miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
      miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
      miner.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      miner.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount

      val response = toResponse(vanillaTx, height)
      miner.getActiveLeases(secondAddress) shouldBe List(response)
      miner.getActiveLeases(firstAddress) shouldBe List(response)

      miner.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("cannot lease non-own waves") {
    for (v <- leaseTxSupportedVersions) {
      val leaseTx   = miner.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val vanillaTx = PBTransactions.vanilla(leaseTx).explicitGet()
      val leaseTxId = vanillaTx.id().toString
      val height    = sender.getStatus(leaseTxId).height

      val secondEffBalance = miner.wavesBalance(secondAddress).effective
      val thirdEffBalance  = miner.wavesBalance(thirdAddress).effective

      assertGrpcError(
        miner.broadcastLease(secondAcc, PBRecipients.create(thirdAcc.toAddress), secondEffBalance - minFee, minFee, version = v),
        errorMessage,
        Code.INVALID_ARGUMENT
      )

      miner.wavesBalance(secondAddress).effective shouldBe secondEffBalance
      miner.wavesBalance(thirdAddress).effective shouldBe thirdEffBalance

      val response = toResponse(vanillaTx, height)
      miner.getActiveLeases(secondAddress) shouldBe List(response)
      miner.getActiveLeases(thirdAddress) shouldBe List.empty

      miner.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("can not make leasing without having enough balance") {
    for (v <- leaseTxSupportedVersions) {
      val firstBalance  = miner.wavesBalance(firstAddress)
      val secondBalance = miner.wavesBalance(secondAddress)

      //secondAddress effective balance more than general balance
      assertGrpcError(
        miner.broadcastLease(secondAcc, Recipient().withPublicKeyHash(firstAddress), secondBalance.regular + 1.waves, minFee, version = v),
        errorMessage,
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        miner.broadcastLease(firstAcc, Recipient().withPublicKeyHash(secondAddress), firstBalance.regular, minFee, version = v),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        miner.broadcastLease(firstAcc, Recipient().withPublicKeyHash(secondAddress), firstBalance.regular - minFee / 2, minFee, version = v),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT
      )

      miner.wavesBalance(firstAddress) shouldBe firstBalance
      miner.wavesBalance(secondAddress) shouldBe secondBalance
      miner.getActiveLeases(firstAddress) shouldBe List.empty
      miner.getActiveLeases(secondAddress) shouldBe List.empty
    }
  }

  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    for (v <- leaseTxSupportedVersions) {
      val firstBalance  = miner.wavesBalance(firstAddress)
      val secondBalance = miner.wavesBalance(secondAddress)

      val leaseTx   = miner.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().toString

      miner.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)

      miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - 2 * minFee
      miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - 2 * minFee
      miner.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      miner.wavesBalance(secondAddress).effective shouldBe secondBalance.effective
      miner.getActiveLeases(secondAddress) shouldBe List.empty
      miner.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

  test("lease cancellation can be done only once") {
    for (v <- leaseTxSupportedVersions) {
      val firstBalance  = miner.wavesBalance(firstAddress)
      val secondBalance = miner.wavesBalance(secondAddress)

      val leaseTx   = miner.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().toString

      miner.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)

      assertGrpcError(
        miner.broadcastLeaseCancel(firstAcc, leaseTxId, minFee),
        "Reason: Cannot cancel already cancelled lease",
        Code.INVALID_ARGUMENT
      )
      miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - 2 * minFee
      miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - 2 * minFee
      miner.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      miner.wavesBalance(secondAddress).effective shouldBe secondBalance.effective

      miner.getActiveLeases(secondAddress) shouldBe List.empty
      miner.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

  test("only sender can cancel lease transaction") {
    for (v <- leaseTxSupportedVersions) {
      val firstBalance  = miner.wavesBalance(firstAddress)
      val secondBalance = miner.wavesBalance(secondAddress)

      val leaseTx   = miner.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val vanillaTx = PBTransactions.vanilla(leaseTx).explicitGet()
      val leaseTxId = vanillaTx.id().toString
      val height    = sender.getStatus(leaseTxId).height

      assertGrpcError(
        miner.broadcastLeaseCancel(secondAcc, leaseTxId, minFee),
        "LeaseTransaction was leased by other sender",
        Code.INVALID_ARGUMENT
      )
      miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
      miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
      miner.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      miner.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount

      val response = toResponse(vanillaTx, height)
      miner.getActiveLeases(secondAddress) shouldBe List(response)
      miner.getActiveLeases(firstAddress) shouldBe List(response)

      miner.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("can not make leasing to yourself") {
    for (v <- leaseTxSupportedVersions) {
      val firstBalance = miner.wavesBalance(firstAddress)
      assertGrpcError(
        miner.broadcastLease(firstAcc, PBRecipients.create(firstAcc.toAddress), leasingAmount, minFee, v),
        "Transaction to yourself",
        Code.INVALID_ARGUMENT
      )
      miner.wavesBalance(firstAddress).regular shouldBe firstBalance.regular
      miner.wavesBalance(firstAddress).effective shouldBe firstBalance.effective
      miner.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

  private def toResponse(tx: Transaction, height: Long): LeaseResponse = {
    val leaseTx   = tx.asInstanceOf[LeaseTransaction]
    val leaseTxId = ByteString.copyFrom(leaseTx.id.value().arr)
    LeaseResponse(
      leaseId = leaseTxId,
      originTransactionId = leaseTxId,
      sender = ByteString.copyFrom(leaseTx.sender.toAddress.bytes),
      recipient = Some(PBRecipients.create(leaseTx.recipient)),
      amount = leaseTx.amount,
      height = height.toInt
    )
  }
}
