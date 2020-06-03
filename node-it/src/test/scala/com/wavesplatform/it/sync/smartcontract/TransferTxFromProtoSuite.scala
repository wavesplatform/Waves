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
import com.wavesplatform.transaction.transfer.TransferTransaction

class TransferTxFromProtoSuite extends BaseTransactionSuite {
  val source    = firstAddress
  val recipient = secondAddress
  val dApp      = thirdAddress
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
    val transferTx = TransferTransaction
      .selfSigned(
        version = TxVersion.V3,
        sender = pkByAddress(source),
        recipient = AddressOrAlias.fromString(recipient).explicitGet(),
        asset = Waves,
        amount = transferAmount,
        feeAsset = Waves,
        fee = minFee,
        attachment = ByteStr("WAVES transfer".getBytes),
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    sender.signedBroadcast(transferTx.json(), waitForTx = true)

    val protoTransferTxBytes = PBTransactions.protobuf(transferTx).toByteArray

    sender.invokeScript(
      source,
      dApp,
      func = Some("foo"),
      args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBytes)).explicitGet()),
      waitForTx = true
    )

    sender.getDataByKey(dApp, "amount").value shouldBe transferTx.amount
    sender.getDataByKey(dApp, "fee").value shouldBe transferTx.fee
    sender.getDataByKey(dApp, "id").value shouldBe transferTx.id().toString
    sender.getDataByKey(dApp, "assetId").value shouldBe "WAVES"
    sender.getDataByKey(dApp, "feeAssetId").value shouldBe "WAVES"
    sender.getDataByKey(dApp, "attachment").value shouldBe Base58.encode("WAVES transfer".getBytes)
    sender.getDataByKey(dApp, "senderPublicKey").value shouldBe transferTx.sender.toString
    sender.getDataByKey(dApp, "sender").value shouldBe transferTx.sender.toAddress.toString
    sender.getDataByKey(dApp, "recipient").value shouldBe transferTx.recipient.toString
    sender.getDataByKey(dApp, "version").value shouldBe transferTx.version
  }

  test("TransferTransaction with issued asset from proto bytes") {
    val assetId = sender.issue(source, waitForTx = true).id
    sender.sponsorAsset(source, assetId, minFee, waitForTx = true)

    val transferAssetTx = TransferTransaction
      .selfSigned(
        version = TxVersion.V3,
        sender = pkByAddress(source),
        recipient = AddressOrAlias.fromString(recipient).explicitGet(),
        asset = IssuedAsset(ByteStr.decodeBase58(assetId).get),
        amount = 10000,
        feeAsset = IssuedAsset(ByteStr.decodeBase58(assetId).get),
        fee = minFee,
        attachment = ByteStr("Some Attachment".getBytes),
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    sender.signedBroadcast(transferAssetTx.json(), waitForTx = true)

    val protoTransferTxBytes = PBTransactions.protobuf(transferAssetTx).toByteArray

    sender.invokeScript(
      source,
      dApp,
      func = Some("foo"),
      args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBytes)).explicitGet()),
      waitForTx = true
    )

    sender.getDataByKey(dApp, "assetId").value shouldBe assetId
    sender.getDataByKey(dApp, "feeAssetId").value shouldBe assetId
  }

  test("check bodyBytes of transaction returned by transferTransactionFromProto") {
    val transferTx = TransferTransaction
      .selfSigned(
        version = TxVersion.V3,
        sender = pkByAddress(source),
        recipient = AddressOrAlias.fromString(recipient).explicitGet(),
        asset = Waves,
        amount = 10000,
        feeAsset = Waves,
        fee = minFee,
        attachment = ByteStr("Some Attachment".getBytes),
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    sender.signedBroadcast(transferTx.json(), waitForTx = true)

    val protoTransferTxBytes = PBTransactions.protobuf(transferTx).toByteArray

    sender.invokeScript(
      source,
      dApp,
      func = Some("foo"),
      args = List(Terms.CONST_BYTESTR(ByteStr(protoTransferTxBytes)).explicitGet()),
      waitForTx = true
    )

    sender.getDataByKey(dApp, "bodyBytes").value.asInstanceOf[ByteStr] shouldBe ByteStr(transferTx.bodyBytes())
  }
}
