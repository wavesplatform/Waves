package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class PseudoTxSenderPubKeySuite extends BaseTransactionSuite {

  private def firstDApp     = firstKeyPair
  private def secondDApp    = secondKeyPair
  private def caller        = thirdKeyPair
  private var firstAssetId  = ""
  private var secondAssetId = ""
  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val smartAssetScript = ScriptCompiler(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |{-# SCRIPT_TYPE ASSET #-}
         |
         |  match tx {
         |    case t: TransferTransaction => t.senderPublicKey == base58'${firstDApp.publicKey.toString}'
         |    case r: ReissueTransaction => r.senderPublicKey == base58'${firstDApp.publicKey.toString}'
         |    case b: BurnTransaction => b.senderPublicKey == base58'${firstDApp.publicKey.toString}'
         |
         |    case _ => throw(tx.senderPublicKey.toBase58String())
         |  }
         """.stripMargin,
      isAssetScript = true,
      ScriptEstimatorV3
    ).explicitGet()._1.bytes().base64
    firstAssetId = sender.issue(firstDApp, fee = issueFee, script = Some(smartAssetScript), waitForTx = true).id
    secondAssetId = sender.issue(secondDApp, fee = issueFee, script = Some(smartAssetScript), waitForTx = true).id

    val dAppScript = ScriptCompiler(
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
         """.stripMargin,
      isAssetScript = false,
      ScriptEstimatorV3
    ).explicitGet()._1.bytes().base64

    sender.setScript(firstDApp, Some(dAppScript), waitForTx = true)
    sender.setScript(secondDApp, Some(dAppScript), waitForTx = true)
  }

  test("check senderPublicKey validation while asset burning") {
    val smartAssetQuantityBefore = sender.assetsDetails(firstAssetId).quantity
    val burnedQuantity           = 100000
    sender.invokeScript(
      caller,
      firstDApp.toAddress.toString,
      func = Some("burnAsset"),
      args = List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(firstAssetId).get).explicitGet(), Terms.CONST_LONG(burnedQuantity)),
      fee = smartMinFee + smartFee,
      waitForTx = true
    )

    sender.assetsDetails(firstAssetId).quantity shouldBe smartAssetQuantityBefore - burnedQuantity
  }

  test("check senderPublicKey validation while asset reissuance") {
    val smartAssetQuantityBefore = sender.assetsDetails(firstAssetId).quantity
    val addedQuantity            = 100000
    sender.invokeScript(
      caller,
      firstDApp.toAddress.toString,
      func = Some("reissueAsset"),
      args =
        List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(firstAssetId).get).explicitGet(), Terms.CONST_BOOLEAN(true), Terms.CONST_LONG(addedQuantity)),
      fee = smartMinFee + smartFee,
      waitForTx = true
    )

    sender.assetsDetails(firstAssetId).quantity shouldBe smartAssetQuantityBefore + addedQuantity
  }

  test("check senderPublicKey validation while asset transfer") {
    val smartAssetBalanceBefore = sender.assetBalance(firstDApp.toAddress.toString, firstAssetId).balance
    sender.invokeScript(
      caller,
      firstDApp.toAddress.toString,
      func = Some("transferAsset"),
      args = List(
        Terms.CONST_BYTESTR(ByteStr.decodeBase58(secondDApp.toAddress.toString).get).explicitGet(),
        Terms.CONST_BYTESTR(ByteStr.decodeBase58(firstAssetId).get).explicitGet(),
        Terms.CONST_LONG(transferAmount)
      ),
      fee = smartMinFee + smartFee,
      waitForTx = true
    )

    sender.assetBalance(firstDApp.toAddress.toString, firstAssetId).balance shouldBe smartAssetBalanceBefore - transferAmount
  }

  test("not able to burn asset if required senderPublicKey didn't match") {
    val smartAssetQuantityBefore = sender.assetsDetails(firstAssetId).quantity
    val burnedQuantity           = 100000
    assertBadRequestAndMessage(
      sender
        .invokeScript(
          caller,
          secondDApp.toAddress.toString,
          func = Some("burnAsset"),
          args = List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(secondAssetId).get).explicitGet(), Terms.CONST_LONG(burnedQuantity)),
          fee = smartMinFee + smartFee
        ),
      "Transaction is not allowed by token-script"
    )

    sender.assetsDetails(secondAssetId).quantity shouldBe smartAssetQuantityBefore
  }

  test("not able to reissue asset if required senderPublicKey didn't match") {
    val smartAssetQuantityBefore = sender.assetsDetails(secondAssetId).quantity
    val addedQuantity            = 100000

    assertBadRequestAndMessage(
      sender
        .invokeScript(
          caller,
          secondDApp.toAddress.toString,
          func = Some("reissueAsset"),
          args = List(
            Terms.CONST_BYTESTR(ByteStr.decodeBase58(secondAssetId).get).explicitGet(),
            Terms.CONST_BOOLEAN(true),
            Terms.CONST_LONG(addedQuantity)
          ),
          fee = smartMinFee + smartFee
        ),
      "Transaction is not allowed by token-script"
    )

    sender.assetsDetails(secondAssetId).quantity shouldBe smartAssetQuantityBefore
  }

  test("not able to transfer asset if required senderPublicKey didn't match") {
    val smartAssetBalanceBefore = sender.assetBalance(firstDApp.toAddress.toString, secondAssetId).balance
    assertBadRequestAndMessage(
      sender
        .invokeScript(
          caller,
          secondDApp.toAddress.toString,
          func = Some("transferAsset"),
          args = List(
            Terms.CONST_BYTESTR(ByteStr.decodeBase58(firstDApp.toAddress.toString).get).explicitGet(),
            Terms.CONST_BYTESTR(ByteStr.decodeBase58(secondAssetId).get).explicitGet(),
            Terms.CONST_LONG(transferAmount)
          ),
          fee = smartMinFee + smartFee
        ),
      "Transaction is not allowed by token-script"
    )

    sender.assetBalance(firstDApp.toAddress.toString, secondAssetId).balance shouldBe smartAssetBalanceBefore
  }
}
