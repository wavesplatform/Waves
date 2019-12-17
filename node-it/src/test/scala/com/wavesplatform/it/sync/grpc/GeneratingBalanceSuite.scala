package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.minFee
import com.wavesplatform.protobuf.transaction.Recipient

class GeneratingBalanceSuite extends GrpcBaseTransactionSuite {

  test("Generating balance should be correct") {
    val amount = 1000000000L

    val senderAddress = ByteString.copyFrom(sender.privateKey.toAddress.bytes)

    val recipient        = KeyPair("recipient".getBytes)
    val recipientAddress = ByteString.copyFrom(recipient.toAddress.bytes)

    val initialBalance = sender.grpc.wavesBalance(senderAddress)

    sender.grpc.broadcastTransfer(sender.privateKey, Recipient().withAddress(recipientAddress), amount, minFee, 2, waitForTx = true)

    val afterTransferBalance = sender.grpc.wavesBalance(senderAddress)

    sender.grpc.broadcastTransfer(recipient, Recipient().withAddress(senderAddress), amount - minFee, minFee, 2, waitForTx = true)

    val finalBalance = sender.grpc.wavesBalance(senderAddress)

    assert(initialBalance.generating <= initialBalance.effective, "initial incorrect")
    assert(afterTransferBalance.generating <= afterTransferBalance.effective, "after transfer incorrect")
    assert(finalBalance.generating <= finalBalance.effective, "final incorrect")
  }

}
