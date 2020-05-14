package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.typesafe.config.Config
import com.wavesplatform.account.{AddressOrAlias, AddressScheme}
import com.wavesplatform.it.{NodeConfigs, WaitForHeight2}
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.protobuf.transaction.{Attachment, PBRecipients, PBSignedTransaction, PBTransaction, PBTransactions, SignedTransaction, TransferTransactionData, Transaction => ProtoTransaction}
import com.wavesplatform.it.api.AsyncNetworkApi.NodeAsyncNetworkApi
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.network.{PBTransactionSpec, RawBytes, TransactionSpec}
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.Recipient.Recipient
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

class InvalidProtobufBytesByNetworkSuite extends FunSuite with Matchers with NodesFromDocker with WaitForHeight2 with CancelAfterFailure {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(3))
      .overrideBase(_.nonMiner)
      .withDefault(3)
      .withSpecial(_.raw("waves.miner.enable = yes"))
      .buildNonConflicting()

  val sender = nodes.head
  val recipient = nodes.last

  test("not able to send protobuf transaction bytes with proofs from vanilla transaction by network") {
    val ts = System.currentTimeMillis()
    val transferTx = TransferTransaction.selfSigned(
      version = TxVersion.V2,
      sender = sender.keyPair,
      recipient = AddressOrAlias.fromString(recipient.address).explicitGet(),
      asset = Waves,
      amount = transferAmount,
      feeAsset = Waves,
      fee = minFee,
      attachment = None,
      timestamp = ts
    ).explicitGet()

    val protoTransferTx = PBSignedTransaction(Some(
      ProtoTransaction(
        AddressScheme.current.chainId,
        ByteString.copyFrom(sender.publicKey.arr),
        Some(Amount.of(ByteString.EMPTY, minFee)),
        ts,
        2,
        PBTransaction.Data.Transfer(
          TransferTransactionData.of(
            Some(PBRecipients.create(AddressOrAlias.fromString(recipient.address).explicitGet())),
            Some(Amount.of(ByteString.EMPTY, transferAmount)),
            None
          )
        )
      )), proofs = Seq(ByteString.copyFrom(transferTx.proofs.bytes())))

    sender.sendByNetwork(RawBytes(PBTransactionSpec.messageCode, protoTransferTx.toByteArray))

    nodes.ensureTxDoesntExist(transferTx.id().toString)

    val id = sender.transfer(sender.address, recipient.address, 100).id
    nodes.waitForHeightAriseAndTxPresent(id)
  }

  test("not able to send protobuf transaction bytes with version '2' by network") {
    val ts = System.currentTimeMillis()

    val unsigned = PBTransaction(
      chainId = AddressScheme.current.chainId,
      senderPublicKey = ByteString.copyFrom(sender.publicKey.arr),
      fee = Some(Amount.of(ByteString.EMPTY, minFee)),
      timestamp = ts,
      version = 2,
      PBTransaction.Data.Transfer(
        TransferTransactionData.of(
          Some(PBRecipients.create(AddressOrAlias.fromString(recipient.address).explicitGet())),
          Some(Amount.of(ByteString.EMPTY, transferAmount)),
          None
        )
      )
    )

    val proofs = crypto.sign(sender.keyPair.privateKey, unsigned.toByteArray)
    val protoTransferTx = SignedTransaction(Some(unsigned), Seq(ByteString.copyFrom(proofs.arr)))
    sender.sendByNetwork(RawBytes(PBTransactionSpec.messageCode, protoTransferTx.toByteArray))

    nodes.ensureTxDoesntExist(PBTransactions.vanilla(protoTransferTx).explicitGet().id().toString)

    val id = sender.transfer(sender.address, recipient.address, 100).id
    nodes.waitForHeightAriseAndTxPresent(id)
  }

}

