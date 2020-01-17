package com.wavesplatform.it.sync.grpc

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions, Recipient}
import io.grpc.Status.Code

class LeasingTransactionsGrpcSuite extends GrpcBaseTransactionSuite {
  private val errorMessage = "Reason: Cannot lease more than own"

  test("leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    for (v <- supportedVersions) {
      val firstBalance     = sender.grpc.wavesBalance(firstAddress)
      val secondBalance    = sender.grpc.wavesBalance(secondAddress)

      val leaseTx = sender.grpc.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().toString

      sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
      sender.grpc.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount

      sender.grpc.getActiveLeases(secondAddress) shouldBe List(leaseTx)
      sender.grpc.getActiveLeases(firstAddress) shouldBe List(leaseTx)

      sender.grpc.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("cannot lease non-own waves") {
    for (v <- supportedVersions) {
      val leaseTx = sender.grpc.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().toString
      val secondEffBalance = sender.grpc.wavesBalance(secondAddress).effective
      val thirdEffBalance = sender.grpc.wavesBalance(thirdAddress).effective

      assertGrpcError(
        sender.grpc.broadcastLease(secondAcc, PBRecipients.create(thirdAcc.toAddress), secondEffBalance - minFee, minFee, version = v),
        errorMessage,
        Code.INVALID_ARGUMENT
      )

      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondEffBalance
      sender.grpc.wavesBalance(thirdAddress).effective shouldBe thirdEffBalance
      sender.grpc.getActiveLeases(secondAddress) shouldBe List(leaseTx)
      sender.grpc.getActiveLeases(thirdAddress) shouldBe List.empty

      sender.grpc.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("can not make leasing without having enough balance") {
    for (v <- supportedVersions) {
      val firstBalance = sender.grpc.wavesBalance(firstAddress)
      val secondBalance = sender.grpc.wavesBalance(secondAddress)

      //secondAddress effective balance more than general balance
      assertGrpcError(
        sender.grpc.broadcastLease(secondAcc, Recipient().withPublicKeyHash(firstAddress), secondBalance.regular + 1.waves, minFee, version = v),
        errorMessage,
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        sender.grpc.broadcastLease(firstAcc, Recipient().withPublicKeyHash(secondAddress), firstBalance.regular, minFee, version = v),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT
      )

      assertGrpcError(
        sender.grpc.broadcastLease(firstAcc, Recipient().withPublicKeyHash(secondAddress), firstBalance.regular - minFee / 2, minFee, version = v),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT
      )

      sender.grpc.wavesBalance(firstAddress) shouldBe firstBalance
      sender.grpc.wavesBalance(secondAddress) shouldBe secondBalance
      sender.grpc.getActiveLeases(firstAddress) shouldBe List.empty
      sender.grpc.getActiveLeases(secondAddress) shouldBe List.empty
    }
  }

  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    for (v <- supportedVersions) {
      val firstBalance = sender.grpc.wavesBalance(firstAddress)
      val secondBalance = sender.grpc.wavesBalance(secondAddress)

      val leaseTx = sender.grpc.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().toString

      sender.grpc.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)

      sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - 2 * minFee
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - 2 * minFee
      sender.grpc.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondBalance.effective
      sender.grpc.getActiveLeases(secondAddress) shouldBe List.empty
      sender.grpc.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

  test("lease cancellation can be done only once") {
    for (v <- supportedVersions) {
      val firstBalance = sender.grpc.wavesBalance(firstAddress)
      val secondBalance = sender.grpc.wavesBalance(secondAddress)

      val leaseTx = sender.grpc.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().toString

      sender.grpc.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)

      assertGrpcError(
        sender.grpc.broadcastLeaseCancel(firstAcc, leaseTxId, minFee),
        "Reason: Cannot cancel already cancelled lease",
        Code.INVALID_ARGUMENT
      )
      sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - 2 * minFee
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - 2 * minFee
      sender.grpc.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondBalance.effective

      sender.grpc.getActiveLeases(secondAddress) shouldBe List.empty
      sender.grpc.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

  test("only sender can cancel lease transaction") {
    for (v <- supportedVersions) {
      val firstBalance = sender.grpc.wavesBalance(firstAddress)
      val secondBalance = sender.grpc.wavesBalance(secondAddress)

      val leaseTx = sender.grpc.broadcastLease(firstAcc, PBRecipients.create(secondAcc.toAddress), leasingAmount, minFee, version = v, waitForTx = true)
      val leaseTxId = PBTransactions.vanilla(leaseTx).explicitGet().id().toString

      assertGrpcError(
        sender.grpc.broadcastLeaseCancel(secondAcc, leaseTxId, minFee),
        "LeaseTransaction was leased by other sender",
        Code.INVALID_ARGUMENT
      )
      sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular - minFee
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective - minFee - leasingAmount
      sender.grpc.wavesBalance(secondAddress).regular shouldBe secondBalance.regular
      sender.grpc.wavesBalance(secondAddress).effective shouldBe secondBalance.effective + leasingAmount
      sender.grpc.getActiveLeases(secondAddress) shouldBe List(leaseTx)
      sender.grpc.getActiveLeases(firstAddress) shouldBe List(leaseTx)

      sender.grpc.broadcastLeaseCancel(firstAcc, leaseTxId, minFee, waitForTx = true)
    }
  }

  test("can not make leasing to yourself") {
    for (v <- supportedVersions) {
      val firstBalance = sender.grpc.wavesBalance(firstAddress)
      assertGrpcError(
        sender.grpc.broadcastLease(firstAcc, PBRecipients.create(firstAcc.toAddress), leasingAmount, minFee, v),
        "ToSelf",
        Code.INTERNAL
      )
      sender.grpc.wavesBalance(firstAddress).regular shouldBe firstBalance.regular
      sender.grpc.wavesBalance(firstAddress).effective shouldBe firstBalance.effective
      sender.grpc.getActiveLeases(firstAddress) shouldBe List.empty
    }
  }

}
