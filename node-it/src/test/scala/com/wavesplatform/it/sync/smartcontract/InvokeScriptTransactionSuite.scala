package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val firstContract  = firstAddress
  private val secondContract = secondAddress
  private val thirdContract  = sender.createAddress()
  private val caller         = thirdAddress

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
        |@Callable(inv)
        | func biz() = [IntegerEntry("numb", 1)]
        |
        """.stripMargin
    val script  = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val script2 = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    sender.transfer(firstAddress, thirdContract, 10.waves, minFee, waitForTx = true)
    val setScriptId  = sender.setScript(firstContract, Some(script), setScriptFee, waitForTx = true).id
    val setScriptId2 = sender.setScript(secondContract, Some(script), setScriptFee, waitForTx = true).id
    sender.setScript(thirdContract, Some(script2), setScriptFee, waitForTx = true).id

    val acc0ScriptInfo  = sender.addressScriptInfo(firstContract)
    val acc0ScriptInfo2 = sender.addressScriptInfo(secondContract)

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
    val arg = ByteStr(Array(42: Byte))
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContract else secondContract
      val invokeScriptTx = sender.invokeScript(
        caller,
        contract,
        func = Some("foo"),
        args = List(CONST_BYTESTR(arg).explicitGet()),
        payment = Seq(Payment(1.waves, Waves)),
        fee = 1.waves,
        version = v,
        waitForTx = true
      )

      nodes.waitForHeightAriseAndTxPresent(invokeScriptTx._1.id)

      sender.getDataByKey(contract, "a") shouldBe BinaryDataEntry("a", arg)
      sender.getDataByKey(contract, "sender") shouldBe BinaryDataEntry("sender", ByteStr.decodeBase58(caller).get)
    }
  }

  test("contract caller invokes a default function on a contract") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContract else secondContract
      val _ = sender.invokeScript(
        caller,
        contract,
        func = None,
        payment = Seq(),
        fee = 1.waves,
        version = v,
        waitForTx = true
      )
      sender.getDataByKey(contract, "a") shouldBe StringDataEntry("a", "b")
      sender.getDataByKey(contract, "sender") shouldBe StringDataEntry("sender", "senderId")
    }
  }

  test("verifier works") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract = if (v < 2) firstContract else secondContract
      val dataTxId = sender.putData(contract, data = List(StringDataEntry("a", "OOO")), fee = 1.waves, waitForTx = true).id

      nodes.waitForHeightAriseAndTxPresent(dataTxId)

      sender.getDataByKey(contract, "a") shouldBe StringDataEntry("a", "OOO")
    }
  }

  test("not able to set an empty key by InvokeScriptTransaction with version >= 2") {
    val tx1 = sender
      .invokeScript(
        caller,
        secondContract,
        func = Some("emptyKey"),
        payment = Seq(),
        fee = 1.waves,
        version = TxVersion.V2,
        waitForTx = true
      )
      ._1
      .id

    sender.debugStateChanges(tx1).stateChanges.get.errorMessage.get.text should include("Empty keys aren't allowed in tx version >= 2")

    nodes.waitForHeightArise()
    sender.getData(secondContract).filter(_.key.isEmpty) shouldBe List.empty

    val tx2 = sender
      .invokeScript(
        caller,
        thirdContract,
        func = Some("bar"),
        payment = Seq(),
        fee = 1.waves,
        version = TxVersion.V2,
        waitForTx = true
      )
      ._1
      .id

    sender.debugStateChanges(tx2).stateChanges.get.errorMessage.get.text should include("Empty keys aren't allowed in tx version >= 2")

    nodes.waitForHeightArise()
    sender.getData(thirdContract).filter(_.key.isEmpty) shouldBe List.empty
  }

  test("invoke script via dApp alias") {
    sender.createAlias(thirdContract, "dappalias", smartMinFee, waitForTx = true)
    val dAppAlias = sender.aliasByAddress(thirdContract).find(_.endsWith("dappalias")).get
    for (v <- invokeScrTxSupportedVersions) {
      sender.invokeScript(caller, dAppAlias, fee = smartMinFee + smartFee, func = Some("biz"), version = v, waitForTx = true)
      sender.getDataByKey(thirdContract, "numb") shouldBe IntegerDataEntry("numb", 1)
    }
  }
}
