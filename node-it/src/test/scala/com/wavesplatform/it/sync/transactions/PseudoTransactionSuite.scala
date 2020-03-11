package com.wavesplatform.it.sync.transactions

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.compiler.Terms

class PseudoTransactionSuite extends BaseTransactionSuite {

  val firstDApp = firstAddress
  val secondDApp = secondAddress
  val caller = thirdAddress
  var smartAssetId = ""
  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val smartAssetScript = ScriptCompiler(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |{-# SCRIPT_TYPE ASSET #-}
         |
         |  match tx {
         |    case t: TransferTransaction => t.senderPublicKey == base58'${pkByAddress(firstDApp).publicKey.toString}'
         |    case r: ReissueTransaction => r.senderPublicKey == base58'${pkByAddress(firstDApp).publicKey.toString}'
         |    case b: BurnTransaction => b.senderPublicKey == base58'${pkByAddress(firstDApp).publicKey.toString}'
         |
         |    case _ => throw(tx.senderPublicKey.toBase58String())
         |  }
         """.stripMargin,
      isAssetScript = true,
      ScriptEstimatorV3
    ).explicitGet()._1.bytes.value.base64
    smartAssetId = sender.issue(firstDApp, fee = issueFee, script = Some(smartAssetScript), waitForTx = true).id

    val dAppScript = ScriptCompiler(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |@Callable (i)
         |func reissueAsset(a: ByteVector, r: Boolean, q: Int) = [Reissue(a, r, q)]
         |
         |@Callable (i)
         |func burnAsset(a: ByteVector, q: Int) = [Burn(a, q)]
         |
         |@Callable (i)
         |func transferAsset(a: ByteVector, q: Int) = [ScriptTransfer(Address(fromBase58String("$secondDApp")), 100, fromBase58String("$smartAssetId"))]
         """.stripMargin,
      isAssetScript = false,
      ScriptEstimatorV3
    ).explicitGet()._1.bytes.value.base64

    sender.setScript(firstDApp, Some(dAppScript), waitForTx = true)
  }

  test("check addressPublicKey validation while asset burning") {
    val smartAssetQuantityBefore = sender.assetsDetails(smartAssetId).quantity
    sender.invokeScript(
      caller,
      firstDApp,
      func = Some("burnAsset"),
      args = List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(smartAssetId).get).explicitGet(), Terms.CONST_LONG(100000)),
      fee = smartMinFee + smartFee,
      waitForTx = true
    )

    sender.assetsDetails(smartAssetId).quantity shouldBe smartAssetQuantityBefore - 100000
  }

  test("check addressPublicKey validation while asset reissuance") {
    sender.invokeScript(
      caller,
      firstDApp,
      func = Some("reissueAsset"),
      args = List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(smartAssetId).get).explicitGet(), Terms.CONST_BOOLEAN(true), Terms.CONST_LONG(someAssetAmount + 1)),
      fee = smartMinFee + smartFee,
      waitForTx = true
    )

    sender.assetsDetails(smartAssetId).quantity shouldBe someAssetAmount + 1
  }



}
