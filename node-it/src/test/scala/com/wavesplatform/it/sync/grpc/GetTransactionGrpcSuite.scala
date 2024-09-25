package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions, Recipient}

class GetTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("get transaction by sender, by recipient, by sender&recipient and id") {
    val txId = PBTransactions
      .vanilla(
        sender.broadcastTransfer(firstAcc, Recipient().withPublicKeyHash(secondAddress), transferAmount, minFee, waitForTx = true),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString
    val transactionBySenderAndId = sender.getTransaction(sender = firstAddress, id = txId).getWavesTransaction
    val transactionByRecipientAndId =
      sender.getTransaction(recipient = Some(Recipient().withPublicKeyHash(secondAddress)), id = txId).getWavesTransaction
    val transactionBySenderRecipientAndId =
      sender.getTransaction(sender = firstAddress, recipient = Some(Recipient().withPublicKeyHash(secondAddress)), id = txId).getWavesTransaction

    transactionBySenderAndId.senderPublicKey shouldBe ByteString.copyFrom(Base58.decode(firstAcc.publicKey.toString))
    transactionByRecipientAndId.getTransfer.getRecipient shouldBe PBRecipients.create(secondAcc.toAddress)
    transactionBySenderRecipientAndId.senderPublicKey shouldBe ByteString.copyFrom(Base58.decode(firstAcc.publicKey.toString))
    transactionBySenderRecipientAndId.getTransfer.getRecipient shouldBe PBRecipients.create(secondAcc.toAddress)
  }

  test("get multiple transactions") {
    val txs =
      List.fill(10)(sender.broadcastTransfer(thirdAcc, Recipient().withPublicKeyHash(secondAddress), transferAmount / 10, minFee, waitForTx = true))
    val txsIds = txs.map(tx => PBTransactions.vanilla(tx, unsafe = false).explicitGet().id().toString)

    val transactionsByIds = sender.getTransactionSeq(txsIds, sender = thirdAddress, recipient = Some(Recipient().withPublicKeyHash(secondAddress)))
    transactionsByIds.size shouldBe 10
    for (tx <- transactionsByIds) {
      tx.getTransaction.getWavesTransaction.senderPublicKey shouldBe ByteString.copyFrom(thirdAcc.publicKey.arr)
      tx.getTransaction.getWavesTransaction.getTransfer.getRecipient shouldBe PBRecipients.create(secondAcc.toAddress)
    }
  }
}
