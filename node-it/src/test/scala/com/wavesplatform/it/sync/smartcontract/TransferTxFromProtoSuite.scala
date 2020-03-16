package com.wavesplatform.it.sync.smartcontract

import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{Proofs, TxVersion}
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}

class TransferTxFromProtoSuite extends BaseTransactionSuite {
  val source = firstAddress
  val recipient = secondAddress
  val dApp = thirdAddress
  val scriptText =
    """
      |{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |@Callable(i)
      |func foo(txProtoBytes: ByteVector) = {
      |    let transferTx = transferTransactionFromProto(txProtoBytes).value()
      |    let transferTxAttachment = match (transferTx.attachment.value()) {
      |        case bool:Boolean => bool.toString()
      |        case b:ByteVector => b.toBase58String()
      |        case integer:Int => integer.toString()
      |        case s:String => s
      |      }
      |[
      |IntegerEntry("amount", transferTx.amount),
      |StringEntry("senderPublicKey", transferTx.senderPublicKey.toBase58String()),
      |StringEntry("sender", transferTx.sender.bytes.toBase58String()),
      |StringEntry("recipient", transferTx.recipient.value().bytes.toBase58String()),
      |StringEntry("assetId", transferTx.assetId.value().toBase58String()),
      |StringEntry("attachment", transferTxAttachment),
      |IntegerEntry("fee", transferTx.fee),
      |StringEntry("feeAssetId", transferTx.feeAssetId.value().toBase58String()),
      |StringEntry("id", transferTx.id.toBase58String())
      |]
      |}
      |""".stripMargin
  val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()._1.bytes().base64

  test("able to get TransferTransaction from proto bytes") {
    sender.setScript(dApp, Some(script), waitForTx = true)
    val transferTx = TransferTransaction.selfSigned(
      version = TxVersion.V3,
      sender = pkByAddress(source),
      recipient = AddressOrAlias.fromString(recipient).explicitGet(),
      asset = Waves,
      amount = transferAmount,
      feeAsset = Waves,
      fee = minFee,
      attachment = Some(Attachment.Str("Some Attachment")),
      timestamp = System.currentTimeMillis()
    ).explicitGet()

    sender.signedBroadcast(transferTx.json(), waitForTx = true)

    val protoTransferTxBytes = PBTransactions.protobuf(transferTx).toByteArray

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBytes)).explicitGet()), waitForTx = true)

    sender.getDataByKey(dApp, "amount").value shouldBe transferTx.amount
    sender.getDataByKey(dApp, "fee").value shouldBe transferTx.fee
    sender.getDataByKey(dApp, "id").value shouldBe Base58.encode(transferTx.id())
    sender.getDataByKey(dApp, "assetId").value shouldBe Some(transferTx.assetId.maybeBase58Repr)
    sender.getDataByKey(dApp, "feeAssetId").value shouldBe Some(transferTx.feeAssetId.maybeBase58Repr)
    sender.getDataByKey(dApp, "attachment").value shouldBe Some(transferTx.attachment.toString)
    sender.getDataByKey(dApp, "senderPublicKey").value shouldBe transferTx.sender.toString
    sender.getDataByKey(dApp, "sender").value shouldBe transferTx.sender.stringRepr
    sender.getDataByKey(dApp, "recipient").value shouldBe transferTx.recipient.toString

  }

}
