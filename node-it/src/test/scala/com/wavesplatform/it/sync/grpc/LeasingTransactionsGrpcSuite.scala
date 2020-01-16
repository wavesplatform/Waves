package com.wavesplatform.it.sync.grpc

import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions, Recipient}
import io.grpc.Status.Code

class LeasingTransactionsGrpcSuite extends GrpcBaseTransactionSuite {
  private val errorMessage = "Reason: Cannot lease more than own"

  test("leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    for (v <- supportedVersions) {
      val firstBalance     = sender.wavesBalance(firstAddress)
      val secondBalance    = sender.wavesBalance(secondAddress)

      val leaseTx = sender.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().base58

      sender.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
      sender.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
      sender.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount

      sender.getActiveLeases(secondAddress) shouldBe List(leaseTx)
      sender.getActiveLeases(firstAddress) shouldBe List(leaseTx)

      sender.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("cannot lease non-own waves") {
    for (v <- supportedVersions) {
      val leaseTx = sender.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().base58
      val secondEffBalance = sender.wavesBalance(secondAddress).effective
      val thirdEffBalance = sender.wavesBalance(thirdAddress).effective

      assertGrpcError(
        sender.broadcastLease(secondAcc, PBRecipients.create(thirdAcc.toAddress), secondEffBalance - minFee, minFee, version = v),
        errorMessage,
        Code.INVALID_ARGUMENT
      )

      sender.wavesBalance(secondAddress).effective shouldBe secondEffBalance
      sender.wavesBalance(thirdAddress).effective shouldBe thirdEffBalance
      sender.getActiveLeases(secondAddress) shouldBe List(leaseTx)
      sender.getActiveLeases(thirdAddress) shouldBe List.empty

      sender.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("can not make leasing without having enough balance") {
    for (v <- supportedVersions) {
      val firstBalance = sender.wavesBalance(firstAddress)
      val secondBalance = sender.wavesBalance(secondAddress)

      //secondAddress effective balance more than general balance
      assertGrpcError(
        sender.broadcastLease(secondAcc, Recipient().withAddress(firstAddress), secondBalance.regular + 1.waves, minFee, version = v),
        errorMessage,
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        sender.broadcastLease(firstAcc, Recipient().withAddress(secondAddress), firstBalance.regular, minFee, version = v),
        "Reason: negative effective balance",
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        sender.broadcastLease(firstAcc, Recipient().withAddress(secondAddress), firstBalance.regular - minFee / 2, minFee, version = v),
        "Reason: negative effective balance",
        Code.INVALID_ARGUMENT
      )

      sender.wavesBalance(firstAddress) shouldBe firstBalance
      sender.wavesBalance(secondAddress) shouldBe secondBalance
      sender.getActiveLeases(firstAddress) shouldBe List.empty
      sender.getActiveLeases(secondAddress) shouldBe List.empty
    }
  }

  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    for (v <- supportedVersions) {
      val firstBalance = sender.wavesBalance(firstAddress)
      val secondBalance = sender.wavesBalance(secondAddress)

      val leaseTx = sender.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().base58

      sender.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)

      sender.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - 2 * minFee
      sender.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - 2 * minFee
      sender.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.wavesBalance(secondAddress).effective shouldBe secondBalance.effective
      sender.getActiveLeases(secondAddress) shouldBe List.empty
      sender.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

  test("lease cancellation can be done only once") {
    for (v <- supportedVersions) {
      val firstBalance = sender.wavesBalance(firstAddress)
      val secondBalance = sender.wavesBalance(secondAddress)

      val leaseTx = sender.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().base58

      sender.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)

      assertGrpcError(
        sender.broadcastLeaseCancel(firstAcc, leaseTxId, minFee),
        "Reason: Cannot cancel already cancelled lease",
        Code.INVALID_ARGUMENT
      )
      sender.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - 2 * minFee
      sender.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - 2 * minFee
      sender.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.wavesBalance(secondAddress).effective shouldBe secondBalance.effective

      sender.getActiveLeases(secondAddress) shouldBe List.empty
      sender.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

  test("only sender can cancel lease transaction") {
    for (v <- supportedVersions) {
      val firstBalance = sender.wavesBalance(firstAddress)
      val secondBalance = sender.wavesBalance(secondAddress)

      val leaseTx = sender.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().base58

      assertGrpcError(
        sender.broadcastLeaseCancel(secondAcc, leaseTxId, minFee),
        "LeaseTransaction was leased by other sender",
        Code.INVALID_ARGUMENT
      )
      sender.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
      sender.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
      sender.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount
      sender.getActiveLeases(secondAddress) shouldBe List(leaseTx)
      sender.getActiveLeases(firstAddress) shouldBe List(leaseTx)

      sender.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("can not make leasing to yourself") {
    for (v <- supportedVersions) {
      val firstBalance = sender.wavesBalance(firstAddress)
      assertGrpcError(
        sender.broadcastLease(firstAcc, PBRecipients.create(firstAcc.toAddress), leasingAmount, minFee, v),
        "ToSelf",
        Code.INTERNAL
      )
      sender.wavesBalance(firstAddress).regular shouldBe firstBalance.regular
      sender.wavesBalance(firstAddress).effective shouldBe firstBalance.effective
      sender.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

}
