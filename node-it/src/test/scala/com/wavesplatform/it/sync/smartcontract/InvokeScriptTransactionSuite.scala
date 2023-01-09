package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state._
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration._

class InvokeScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  val activationHeight = 8
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(NodeConfigs.Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.Ride4DApps.id, 0),
          (BlockchainFeatures.BlockV5.id, activationHeight)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  private def firstContract      = firstKeyPair
  private def secondContract     = secondKeyPair
  private lazy val thirdContract = sender.createKeyPair()
  private def caller             = thirdKeyPair

  private lazy val firstContractAddress: String  = firstContract.toAddress.toString
  private lazy val secondContractAddress: String = secondContract.toAddress.toString

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
        | func baz() = {
        |  WriteSet([DataEntry("test", this.bytes)])
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
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.transfer(firstKeyPair, thirdContract.toAddress.toString, 10.waves, minFee, waitForTx = true)
    val setScriptId  = sender.setScript(firstContract, Some(script), setScriptFee, waitForTx = true).id
    val setScriptId2 = sender.setScript(secondContract, Some(script), setScriptFee, waitForTx = true).id

    val acc0ScriptInfo  = sender.addressScriptInfo(firstContractAddress)
    val acc0ScriptInfo2 = sender.addressScriptInfo(secondContractAddress)
    sender.createAlias(firstContract, "alias", fee = 1.waves, waitForTx = true)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true
    acc0ScriptInfo2.script.isEmpty shouldBe false
    acc0ScriptInfo2.scriptText.isEmpty shouldBe false
    acc0ScriptInfo2.script.get.startsWith("base64:") shouldBe true

    sender.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true
    sender.transactionInfo[TransactionInfo](setScriptId2).script.get.startsWith("base64:") shouldBe true
  }

  ignore("""Allow to use "this" if DApp is called by alias""") {
    sender.invokeScript(
      caller,
      "alias:I:alias",
      func = Some("baz"),
      args = List(),
      payment = Seq(),
      fee = 1.waves,
      waitForTx = true
    )
    sender.getDataByKey(firstContractAddress, "test") shouldBe BinaryDataEntry("test", ByteStr(firstContract.toAddress.bytes))
  }

  test("Wait for activation") {
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
    val script2 = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3.latest).explicitGet()._1.bytes().base64
    sender.waitForHeight(activationHeight, 13.minute)
    val setScriptId3 = sender.setScript(thirdContract, Some(script2), setScriptFee, waitForTx = true).id
    sender.transactionInfo[TransactionInfo](setScriptId3).script.get.startsWith("base64:") shouldBe true
  }

  test("contract caller invokes a function on a contract") {
    val arg = ByteStr(Array(42: Byte))
    for (v <- invokeScrTxSupportedVersions) {
      val contract = (if (v < 2) firstContract else secondContract).toAddress.toString
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
      sender.getDataByKey(contract, "sender") shouldBe BinaryDataEntry("sender", ByteStr(caller.toAddress.bytes))
    }
  }

  test("contract caller invokes a function on a contract by alias") {
    val arg = ByteStr(Array(43: Byte))

    val _ = sender.invokeScript(
      caller,
      "alias:I:alias",
      func = Some("foo"),
      args = List(CONST_BYTESTR(arg).explicitGet()),
      payment = Seq(),
      fee = 1.waves,
      waitForTx = true
    )

    sender.getDataByKey(firstContractAddress, "a") shouldBe BinaryDataEntry("a", arg)
    sender.getDataByKey(firstContractAddress, "sender") shouldBe BinaryDataEntry("sender", ByteStr(caller.toAddress.bytes))
  }

  test("translate alias to the address") {
    sender.invokeScript(
      caller,
      "alias:I:alias",
      func = Some("baz"),
      args = List(),
      payment = Seq(),
      fee = 1.waves,
      waitForTx = true
    )
    sender.getDataByKey(firstContractAddress, "test") shouldBe BinaryDataEntry("test", ByteStr(firstContract.toAddress.bytes))
  }

  test("contract caller invokes a default function on a contract") {
    for (v <- invokeScrTxSupportedVersions) {
      val contract = (if (v < 2) firstContract else secondContract).toAddress.toString
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

      sender.getDataByKey(contract.toAddress.toString, "a") shouldBe StringDataEntry("a", "OOO")
    }
  }

  test("not able to set an empty key by InvokeScriptTransaction with version >= 2") {
    assertApiError(
      sender
        .invokeScript(
          caller,
          secondContractAddress,
          func = Some("emptyKey"),
          payment = Seq(),
          fee = 1.waves,
          version = TxVersion.V2
        ),
      AssertiveApiError(ScriptExecutionError.Id, "Error while executing dApp: Empty keys aren't allowed in tx version >= 2")
    )

    nodes.waitForHeightArise()
    sender.getData(secondContractAddress).filter(_.key.isEmpty) shouldBe List.empty

    assertApiError(
      sender.invokeScript(
        caller,
        thirdContract.toAddress.toString,
        func = Some("bar"),
        payment = Seq(),
        fee = 1.waves,
        version = TxVersion.V2
      ),
      AssertiveApiError(ScriptExecutionError.Id, "Error while executing dApp: Empty keys aren't allowed in tx version >= 2")
    )

    nodes.waitForHeightArise()
    sender.getData(thirdContract.toAddress.toString).filter(_.key.isEmpty) shouldBe List.empty
  }

  test("invoke script via dApp alias") {
    sender.createAlias(thirdContract, "dappalias", smartMinFee, waitForTx = true)
    val dAppAlias = sender.aliasByAddress(thirdContract.toAddress.toString).find(_.endsWith("dappalias")).get
    for (v <- invokeScrTxSupportedVersions) {
      sender.invokeScript(caller, dAppAlias, fee = smartMinFee + smartFee, func = Some("biz"), version = v, waitForTx = true)
      sender.getDataByKey(thirdContract.toAddress.toString, "numb") shouldBe IntegerDataEntry("numb", 1)
    }
  }
}
