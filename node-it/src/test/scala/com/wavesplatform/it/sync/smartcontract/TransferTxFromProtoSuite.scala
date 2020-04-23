package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.Attachment.{Bin, Bool, Num, Str}
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
      |    let transferTxAttachment = match (transferTx.attachment) {
      |        case b:ByteVector => b.toBase58String()
      |        case integer:Int => integer.toString()
      |        case bool:Boolean => bool.toString()
      |        case s:String => s
      |        case _ => throw("Empty description")
      |      }
      |    let assetId = if (!transferTx.assetId.isDefined()) then {"WAVES"} else {transferTx.assetId.value().toBase58String()}
      |    let feeAssetId = if (!transferTx.feeAssetId.isDefined()) then {"WAVES"} else {transferTx.feeAssetId.value().toBase58String()}
      |[
      |IntegerEntry("amount", transferTx.amount),
      |StringEntry("senderPublicKey", transferTx.senderPublicKey.toBase58String()),
      |StringEntry("sender", transferTx.sender.bytes.toBase58String()),
      |StringEntry("recipient", addressFromRecipient(transferTx.recipient).bytes.toBase58String()),
      |StringEntry("assetId", assetId),
      |StringEntry("attachment", transferTxAttachment),
      |IntegerEntry("fee", transferTx.fee),
      |StringEntry("feeAssetId", feeAssetId),
      |StringEntry("id", transferTx.id.toBase58String()),
      |IntegerEntry("version", transferTx.version),
      |BinaryEntry("bodyBytes",transferTx.bodyBytes)
      |]
      |}
      |
      |""".stripMargin
  val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()._1.bytes().base64

  test("TransferTransaction with Waves from proto bytes") {
    sender.setScript(dApp, Some(script), waitForTx = true)
    val transferTx = TransferTransaction.selfSigned(
      version = TxVersion.V3,
      sender = pkByAddress(source),
      recipient = AddressOrAlias.fromString(recipient).explicitGet(),
      asset = Waves,
      amount = transferAmount,
      feeAsset = Waves,
      fee = minFee,
      attachment = Some(Attachment.Str("WAVES transfer")),
      timestamp = System.currentTimeMillis()
    ).explicitGet()

    sender.signedBroadcast(transferTx.json(), waitForTx = true)

    val protoTransferTxBytes = PBTransactions.protobuf(transferTx).toByteArray

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBytes)).explicitGet()), waitForTx = true)

    sender.getDataByKey(dApp, "amount").value shouldBe transferTx.amount
    sender.getDataByKey(dApp, "fee").value shouldBe transferTx.fee
    sender.getDataByKey(dApp, "id").value shouldBe Base58.encode(transferTx.id())
    sender.getDataByKey(dApp, "assetId").value shouldBe "WAVES"
    sender.getDataByKey(dApp, "feeAssetId").value shouldBe "WAVES"
    sender.getDataByKey(dApp, "attachment").value shouldBe "WAVES transfer"
    sender.getDataByKey(dApp, "senderPublicKey").value shouldBe transferTx.sender.toString
    sender.getDataByKey(dApp, "sender").value shouldBe transferTx.sender.stringRepr
    sender.getDataByKey(dApp, "recipient").value shouldBe transferTx.recipient.toString
    sender.getDataByKey(dApp, "version").value shouldBe transferTx.version
  }

  test("TransferTransaction with issued asset from proto bytes") {
    val assetId = sender.issue(source, waitForTx = true).id
    sender.sponsorAsset(source, assetId, minFee, waitForTx = true)

    val transferAssetTx = TransferTransaction.selfSigned(
      version = TxVersion.V3,
      sender = pkByAddress(source),
      recipient = AddressOrAlias.fromString(recipient).explicitGet(),
      asset = IssuedAsset(Base58.decode(assetId)),
      amount = 10000,
      feeAsset = IssuedAsset(Base58.decode(assetId)),
      fee = minFee,
      attachment = Some(Attachment.Str("Some Attachment")),
      timestamp = System.currentTimeMillis()
    ).explicitGet()

    sender.signedBroadcast(transferAssetTx.json(), waitForTx = true)

    val protoTransferTxBytes = PBTransactions.protobuf(transferAssetTx).toByteArray

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBytes)).explicitGet()), waitForTx = true)

    sender.getDataByKey(dApp, "assetId").value shouldBe assetId
    sender.getDataByKey(dApp, "feeAssetId").value shouldBe assetId
  }

  test("check bodyBytes of transaction returned by transferTransactionFromProto") {
    val transferTx = TransferTransaction.selfSigned(
      version = TxVersion.V3,
      sender = pkByAddress(source),
      recipient = AddressOrAlias.fromString(recipient).explicitGet(),
      asset = Waves,
      amount = 10000,
      feeAsset = Waves,
      fee = minFee,
      attachment = Some(Attachment.Str("Some Attachment")),
      timestamp = System.currentTimeMillis()
    ).explicitGet()

    sender.signedBroadcast(transferTx.json(), waitForTx = true)

    val protoTransferTxBytes = PBTransactions.protobuf(transferTx).toByteArray

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBytes)).explicitGet()), waitForTx = true)

    sender.getDataByKey(dApp, "bodyBytes").value.asInstanceOf[ByteStr] shouldBe ByteStr(transferTx.bodyBytes())
  }

  test("TransferTransaction with different typed attachments from proto bytes") {
    def transferTx(attachment: Option[Attachment]): TransferTransaction = TransferTransaction.selfSigned(
      version = TxVersion.V3,
      sender = pkByAddress(source),
      recipient = AddressOrAlias.fromString(recipient).explicitGet(),
      asset = Waves,
      amount = transferAmount,
      feeAsset = Waves,
      fee = minFee,
      attachment = attachment,
      timestamp = System.currentTimeMillis()
    ).explicitGet()
    val transferTxWithStrAttachment = transferTx(Some(Attachment.Str("Some String")))
    val transferTxWithBinAttachment = transferTx(Some(Attachment.Bin(Array[Byte](127.toByte, 0, 1, 1))))
    val transferTxWithNumAttachment = transferTx(Some(Attachment.Num(123)))
    val transferTxWithBoolAttachment = transferTx(Some(Attachment.Bool(true)))
    val transferTxWithNoneAttachment = transferTx(None)

    sender.signedBroadcast(transferTxWithStrAttachment.json(), waitForTx = true)
    sender.signedBroadcast(transferTxWithBinAttachment.json(), waitForTx = true)
    sender.signedBroadcast(transferTxWithNumAttachment.json(), waitForTx = true)
    sender.signedBroadcast(transferTxWithBoolAttachment.json(), waitForTx = true)
    sender.signedBroadcast(transferTxWithNoneAttachment.json(), waitForTx = true)

    val protoTransferTxStrAttBytes = PBTransactions.protobuf(transferTxWithStrAttachment).toByteArray
    val protoTransferTxBinAttBytes = PBTransactions.protobuf(transferTxWithBinAttachment).toByteArray
    val protoTransferTxNumAttBytes = PBTransactions.protobuf(transferTxWithNumAttachment).toByteArray
    val protoTransferTxBoolAttBytes = PBTransactions.protobuf(transferTxWithBoolAttachment).toByteArray
    val protoTransferTxNoneAttBytes = PBTransactions.protobuf(transferTxWithNoneAttachment).toByteArray

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxStrAttBytes)).explicitGet()), waitForTx = true)
    sender.getDataByKey(dApp, "attachment").value shouldBe transferTxWithStrAttachment.attachment.get.asInstanceOf[Str].value

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBinAttBytes)).explicitGet()), waitForTx = true)
    sender.getDataByKey(dApp, "attachment").value shouldBe Base58.encode(transferTxWithBinAttachment.attachment.get.asInstanceOf[Bin].value)

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxNumAttBytes)).explicitGet()), waitForTx = true)
    sender.getDataByKey(dApp, "attachment").value shouldBe s"${transferTxWithNumAttachment.attachment.get.asInstanceOf[Num].value}"

    sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBoolAttBytes)).explicitGet()), waitForTx = true)
    sender.getDataByKey(dApp, "attachment").value shouldBe s"${transferTxWithBoolAttachment.attachment.get.asInstanceOf[Bool].value}"

    val tx =
      sender.invokeScript(source, dApp, func = Some("foo"), args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxNoneAttBytes)).explicitGet()), waitForTx = true)
          ._1.id
    sender.debugStateChanges(tx).stateChanges.get.errorMessage.get.text should include("Empty description")
  }
}
