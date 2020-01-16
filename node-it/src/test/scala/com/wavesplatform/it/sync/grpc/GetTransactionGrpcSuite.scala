package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBSignedTransaction, PBTransactions, Recipient}
import com.wavesplatform.it.sync._
import com.wavesplatform.common.utils.{Base58, EitherExt2}

class GetTransactionGrpcSuite extends GrpcBaseTransactionSuite {

  test("get transaction by sender, by recipient, by sender&recipient and id") {
    val txId = PBTransactions.vanilla(
      sender.broadcastTransfer(firstAcc, Recipient().withAddress(secondAddress), transferAmount, minFee, waitForTx = true)
    ).explicitGet().id().base58
    val transactionBySenderAndId = sender.getTransaction(sender = firstAddress, id = txId).getTransaction
    val transactionByRecipientAndId = sender.getTransaction(recipient = Some(Recipient().withAddress(secondAddress)), id = txId).getTransaction
    val transactionBySenderRecipientAndId = sender.getTransaction(sender = firstAddress, recipient = Some(Recipient().withAddress(secondAddress)), id = txId).getTransaction

    transactionBySenderAndId.senderPublicKey shouldBe ByteString.copyFrom(Base58.decode(firstAcc.publicKey.base58))
    transactionByRecipientAndId.getTransfer.getRecipient shouldBe PBRecipients.create(secondAcc.toAddress)
    transactionBySenderRecipientAndId.senderPublicKey shouldBe ByteString.copyFrom(Base58.decode(firstAcc.publicKey.base58))
    transactionBySenderRecipientAndId.getTransfer.getRecipient shouldBe PBRecipients.create(secondAcc.toAddress)
  }

  test("get multiple transactions") {
    val txs = List.fill(10)(sender.broadcastTransfer(firstAcc, Recipient().withAddress(secondAddress), transferAmount / 10, minFee, waitForTx = true))
    val txsIds = txs.map(tx => PBTransactions.vanilla(tx).explicitGet().id().base58)

    val transactionsByIds = sender.getTransactionSeq(txsIds, sender = firstAddress, recipient = Some(Recipient().withAddress(secondAddress)))
    transactionsByIds.size shouldBe 10
    for(tx <- transactionsByIds) {
      tx.getTransaction.senderPublicKey shouldBe ByteString.copyFrom(Base58.decode(firstAcc.publicKey.base58))
      tx.getTransaction.getTransfer.getRecipient shouldBe PBRecipients.create(secondAcc.toAddress)
    }
  }

}
