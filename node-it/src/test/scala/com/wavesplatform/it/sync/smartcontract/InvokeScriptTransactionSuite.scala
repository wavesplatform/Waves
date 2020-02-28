package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync.{minFee, setScriptFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{DataTransaction, Proofs, TxVersion}
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber

class InvokeScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val firstContract = pkByAddress(firstAddress)
  private val secondContract = pkByAddress(secondAddress)
  private val thirdContract = pkByAddress(sender.createAddress())
  private val caller   = pkByAddress(thirdAddress)

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val scriptText =
      """
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        | func foo(a:ByteVector) = {
        |  WriteSet([DataEntry("a", a), DataEntry("sender", inv.caller.bytes)])
        | }
        | @Callable(inv)
        | func emptyKey() = {
        |  WriteSet([DataEntry("", "a")])
        | }
        |
        | @Callable(inv)
        | func default() = {
        |  WriteSet([DataEntry("a", "b"), DataEntry("sender", "senderId")])
        | }
        | 
        | @Verifier(t)
        | func verify() = {
        |  true
        | }
        |
        |
        """.stripMargin
    val scriptTextV4 =
      """
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        |func foo() = [IntegerEntry("", 1)]
        |
        | @Callable(inv)
        |func bar() = [IntegerEntry("", 2)]
        |
        """.stripMargin
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val script2 = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    sender.transfer(firstAddress, thirdContract.stringRepr, 10.waves, minFee, waitForTx = true)
    val setScriptId = sender.setScript(firstContract.stringRepr, Some(script), setScriptFee, waitForTx = true).id
    val setScriptId2 = sender.setScript(secondContract.stringRepr, Some(script), setScriptFee, waitForTx = true).id
    sender.setScript(thirdContract.stringRepr, Some(script2), setScriptFee, waitForTx = true).id

    val acc0ScriptInfo = sender.addressScriptInfo(firstContract.stringRepr)
    val acc0ScriptInfo2 = sender.addressScriptInfo(secondContract.stringRepr)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true
    acc0ScriptInfo2.script.isEmpty shouldBe false
    acc0ScriptInfo2.scriptText.isEmpty shouldBe false
    acc0ScriptInfo2.script.get.startsWith("base64:") shouldBe true

    sender.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true
    sender.transactionInfo[TransactionInfo](setScriptId2).script.get.startsWith("base64:") shouldBe true
  }

  test("contract caller invokes a function on a contract") {
    val arg               = ByteStr(Array(42: Byte))
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContract else secondContract
      val invokeScriptTx = sender.invokeScript(
        caller.stringRepr,
        contract.stringRepr,
        func = Some("foo"),
        args = List(CONST_BYTESTR(arg).explicitGet()),
        payment = Seq(),
        fee = 1.waves,
        waitForTx = true
      )

      nodes.waitForHeightAriseAndTxPresent(invokeScriptTx._1.id)

      sender.getDataByKey(contract.stringRepr, "a") shouldBe BinaryDataEntry("a", arg)
      sender.getDataByKey(contract.stringRepr, "sender") shouldBe BinaryDataEntry("sender", caller.toAddress.bytes)
    }
  }

  test("contract caller invokes a default function on a contract") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContract else secondContract
      val _ = sender.invokeScript(
        caller.stringRepr,
        contract.stringRepr,
        func = None,
        payment = Seq(),
        fee = 1.waves,
        waitForTx = true
      )
      sender.getDataByKey(contract.stringRepr, "a") shouldBe StringDataEntry("a", "b")
      sender.getDataByKey(contract.stringRepr, "sender") shouldBe StringDataEntry("sender", "senderId")
    }
  }

  test("verifier works") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContract else secondContract
      val tx =
        DataTransaction
          .create(1.toByte, sender = contract, data = List(StringDataEntry("a", "OOO")), fee = 1.waves, timestamp = System.currentTimeMillis(), proofs = Proofs.empty)
          .explicitGet()

      val dataTxId = sender
        .signedBroadcast(tx.json() + ("type" -> JsNumber(DataTransaction.typeId.toInt)))
        .id

      nodes.waitForHeightAriseAndTxPresent(dataTxId)

      sender.getDataByKey(contract.stringRepr, "a") shouldBe StringDataEntry("a", "OOO")
    }
  }

  test("not able to set an empty key by InvokeScriptTransaction with version >= 2") {
    assertApiError(
      sender.invokeScript(
        caller.stringRepr,
        secondContract.stringRepr,
        func = Some("emptyKey"),
        payment = Seq(),
        fee = 1.waves,
        version = TxVersion.V2
      ), AssertiveApiError(StateCheckFailed.Id, "State check failed. Reason: Empty keys aren't allowed in tx version >= 2")
    )

    nodes.waitForHeightArise()
    sender.getData(secondContract.stringRepr).filter(_.key.isEmpty) shouldBe List.empty

    assertApiError(
      sender.invokeScript(
        caller.stringRepr,
        thirdContract.stringRepr,
        func = Some("bar"),
        payment = Seq(),
        fee = 1.waves,
        version = TxVersion.V2
      ), AssertiveApiError(StateCheckFailed.Id, "State check failed. Reason: Empty keys aren't allowed in tx version >= 2")
    )

    nodes.waitForHeightArise()
    sender.getData(thirdContract.stringRepr).filter(_.key.isEmpty) shouldBe List.empty
  }
}
