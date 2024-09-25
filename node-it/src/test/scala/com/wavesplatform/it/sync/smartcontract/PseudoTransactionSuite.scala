package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.utils.Signed

class PseudoTransactionSuite extends BaseTransactionSuite {

  private def firstDApp = firstKeyPair

  private def recipient = secondKeyPair

  private def caller = thirdKeyPair

  private var smartAssetId   = ""
  private val recipientAlias = "alias"

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    smartAssetId = sender.issue(firstDApp, fee = issueFee, script = Some(scriptBase64), waitForTx = true).id

    val dAppScript = ScriptCompiler
      .compile(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |
           |@Callable (i)
           |func reissueAsset(a: ByteVector, r: Boolean, q: Int) = [Reissue(a, q, r)]
           |
           |@Callable (i)
           |func burnAsset(a: ByteVector, q: Int) = [Burn(a, q)]
           |
           |@Callable (i)
           |func transferAsset(r: ByteVector, a: ByteVector, q: Int) = [ScriptTransfer(Address(r), q, a)]
           |
           |@Callable (i)
           |func transferAssetByAlias(r: String, a: ByteVector, q: Int) = [ScriptTransfer(Alias(r), q, a)]
         """.stripMargin,
        ScriptEstimatorV3.latest
      )
      .explicitGet()
      ._1
      .bytes()
      .base64

    sender.setScript(firstDApp, Some(dAppScript), waitForTx = true)
    sender.setScript(recipient, Some(dAppScript), waitForTx = true)
  }

  test("check burn pseudotransaction fields") {
    val smartAssetQuantityBefore = sender.assetsDetails(smartAssetId).quantity
    val burnedQuantity           = 100000
    val signedInvoke = invokeScriptTransaction(
      "burnAsset",
      List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(smartAssetId).get).explicitGet(), Terms.CONST_LONG(burnedQuantity))
    )
    sender.setAssetScript(
      smartAssetId,
      firstDApp,
      script = Some(smartAssetScript(signedInvoke.id().toString)),
      fee = issueFee + smartFee,
      waitForTx = true
    )
    sender.signedBroadcast(signedInvoke.json(), waitForTx = true)

    sender.assetsDetails(smartAssetId).quantity shouldBe smartAssetQuantityBefore - burnedQuantity
  }

  test("check reissue pseudotransaction fields") {
    val smartAssetQuantityBefore = sender.assetsDetails(smartAssetId).quantity
    val addedQuantity            = 100000
    val signedInvoke = invokeScriptTransaction(
      "reissueAsset",
      List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(smartAssetId).get).explicitGet(), Terms.CONST_BOOLEAN(true), Terms.CONST_LONG(addedQuantity))
    )
    sender.setAssetScript(
      smartAssetId,
      firstDApp,
      script = Some(smartAssetScript(signedInvoke.id().toString)),
      fee = issueFee + smartFee,
      waitForTx = true
    )
    sender.signedBroadcast(signedInvoke.json(), waitForTx = true)

    sender.assetsDetails(smartAssetId).quantity shouldBe smartAssetQuantityBefore + addedQuantity
  }

  test("check transfer pseudotransaction fields") {
    val signedInvoke = invokeScriptTransaction(
      "transferAsset",
      List(
        Terms.CONST_BYTESTR(ByteStr.decodeBase58(recipient.toAddress.toString).get).explicitGet(),
        Terms.CONST_BYTESTR(ByteStr.decodeBase58(smartAssetId).get).explicitGet(),
        Terms.CONST_LONG(transferAmount / 2)
      )
    )
    sender.setAssetScript(
      smartAssetId,
      firstDApp,
      script = Some(smartAssetScript(signedInvoke.id().toString)),
      fee = issueFee + smartFee,
      waitForTx = true
    )
    sender.signedBroadcast(signedInvoke.json(), waitForTx = true)

    sender.createAlias(recipient, recipientAlias, fee = smartMinFee, waitForTx = true)
    val signedInvoke2 = invokeScriptTransaction(
      "transferAssetByAlias",
      List(
        Terms.CONST_STRING(recipientAlias).explicitGet(),
        Terms.CONST_BYTESTR(ByteStr.decodeBase58(smartAssetId).get).explicitGet(),
        Terms.CONST_LONG(transferAmount / 2)
      )
    )
    sender.setAssetScript(
      smartAssetId,
      firstDApp,
      script = Some(smartAssetScript(signedInvoke2.id().toString)),
      fee = issueFee + smartFee,
      waitForTx = true
    )
    sender.signedBroadcast(signedInvoke2.json(), waitForTx = true)
  }

  private def smartAssetScript(invokeId: String): String =
    TestCompiler.DefaultVersion
      .compileAsset(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# CONTENT_TYPE EXPRESSION #-}
           |{-# SCRIPT_TYPE ASSET #-}
           |
           |  match tx {
           |    case t: TransferTransaction => t.senderPublicKey.toBase58String() == "${firstDApp.publicKey.toString}"
           |     && t.assetId.value().toBase58String() == "$smartAssetId"
           |     && toBase64String(t.attachment) == ""
           |     && t.bodyBytes.size() == 0
           |     && t.fee == 0
           |     && t.feeAssetId == unit
           |     && t.id == fromBase58String("$invokeId")
           |     && (toBase58String(addressFromRecipient(t.recipient).bytes) == "${recipient.toAddress.toString}" ||
           |     toBase58String(addressFromRecipient(t.recipient).bytes) == "$recipientAlias")
           |     && toBase58String(t.sender.bytes) == "${firstDApp.toAddress.toString}"
           |     && t.version == 0
           |    case r: ReissueTransaction => r.senderPublicKey.toBase58String() == "${firstDApp.publicKey.toString}"
           |     && r.assetId.value().toBase58String() == "$smartAssetId"
           |     && r.bodyBytes.size() == 0
           |     && r.fee == 0
           |     && r.id.size() != 0
           |     && toBase58String(r.sender.bytes) == "${firstDApp.toAddress.toString}"
           |     && r.version == 0
           |     && r.quantity == 100000
           |     && r.reissuable == true
           |    case b: BurnTransaction => b.senderPublicKey.toBase58String() == "${firstDApp.publicKey.toString}"
           |     && b.assetId.value().toBase58String() == "$smartAssetId"
           |     && b.bodyBytes.size() == 0
           |     && b.fee == 0
           |     && b.id.size() != 0
           |     && toBase58String(b.sender.bytes) == "${firstDApp.toAddress.toString}"
           |     && b.version == 0
           |     && b.quantity == 100000
           |    case _ => true
           |  }
         """.stripMargin
      )
      .bytes()
      .base64

  private def invokeScriptTransaction(func: String, args: List[EXPR]): InvokeScriptTransaction =
    Signed.invokeScript(
      2.toByte,
      caller,
      AddressOrAlias.fromString(firstDApp.toAddress.toString).explicitGet(),
      Some(FUNCTION_CALL(FunctionHeader.User(func), args)),
      Seq.empty,
      smartMinFee + smartFee,
      Waves,
      System.currentTimeMillis()
    )
}
