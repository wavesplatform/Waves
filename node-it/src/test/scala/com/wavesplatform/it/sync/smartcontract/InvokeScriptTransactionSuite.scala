package com.wavesplatform.it.sync.smartcontract

import com.google.common.primitives.Longs
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.transactions.PriorityTransaction
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class InvokeScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure with PriorityTransaction {
  import restApi._

  private val firstContract  = firstAddress
  private val secondContract = secondAddress
  private val thirdContract  = sender.createAddress()
  private val caller         = thirdAddress

  private val assetAmount    = 1000000000L
  private var smartAsset     = ""
  private var sponsoredAsset = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sender.transfer(sender.address, thirdContract, 100.waves, minFee, waitForTx = true)

    smartAsset = sender
      .issue(
        thirdContract,
        "Asset",
        "Description",
        assetAmount,
        8,
        script = Some(ScriptCompiler.compile("true", ScriptEstimatorV3).explicitGet()._1.bytes().base64),
        waitForTx = true
      )
      .id

    sponsoredAsset = sender
      .issue(
        thirdContract,
        "Sponsored Asset",
        "Description",
        assetAmount,
        8,
        script = None,
        waitForTx = true
      )
      .id

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
      s"""
        |{-# STDLIB_VERSION 4 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |let asset = base58'$smartAsset'
        |
        |@Callable(inv)
        |func foo() = [IntegerEntry("", 1)]
        |
        |@Callable(inv)
        |func bar() = [IntegerEntry("", 2)]
        |
        |@Callable(inv)
        |func biz() = [IntegerEntry("numb", 1)]
        |
        |@Callable(inv)
        |func tikTok() = {
        |  let action = valueOrElse(getString(this, "tikTok"), "unknown")
        |  if (action == "transfer") then [ScriptTransfer(inv.caller, 15, asset)]
        |  else if (action == "issue") then [Issue("new asset", "", 100, 8, true, unit, 0)]
        |  else if (action == "reissue") then [Reissue(asset, true, 15)]
        |  else if (action == "burn") then [Burn(asset, 15)]
        |  else []
        |}
        |
        |@Callable(inv)
        |func transferAndWrite(x: Int) = {
        |  if (x % 4 == 0) then [ScriptTransfer(inv.caller, 15, asset), IntegerEntry("n", x)]
        |  else if (x % 4 == 1) then [ScriptTransfer(inv.caller, 15, asset), BooleanEntry("b", x % 2 == 0)]
        |  else if (x % 4 == 2) then [ScriptTransfer(inv.caller, 15, asset), BinaryEntry("bn", toBytes(x))]
        |  else if (x % 4 == 3) then [ScriptTransfer(inv.caller, 15, asset), StringEntry("s", toString(x))]
        |  else []
        |}
        |
        """.stripMargin
    val script       = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val script2      = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
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
      sender.getDataByKey(contract, "sender") shouldBe BinaryDataEntry("sender", Base58.decode(caller))
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
    assertApiError(
      sender.invokeScript(
        caller,
        secondContract,
        func = Some("emptyKey"),
        payment = Seq(),
        fee = 1.waves,
        version = TxVersion.V2
      ),
      AssertiveApiError(StateCheckFailed.Id, "State check failed. Reason: Empty keys aren't allowed in tx version >= 2")
    )

    nodes.waitForHeightArise()
    sender.getData(secondContract).filter(_.key.isEmpty) shouldBe List.empty

    assertApiError(
      sender.invokeScript(
        caller,
        thirdContract,
        func = Some("bar"),
        payment = Seq(),
        fee = 1.waves,
        version = TxVersion.V2
      ),
      AssertiveApiError(StateCheckFailed.Id, "State check failed. Reason: Empty keys aren't allowed in tx version >= 2")
    )

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

  test("insufficient action fees propagates failed transaction") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAssetScript(result = true, smartAsset, thirdContract, setAssetScriptMinFee)

    for (typeName <- Seq("transfer", "issue", "reissue", "burn")) {
      updateTikTok("unknown", setAssetScriptMinFee)

      val prevBalance = sender.balance(caller).balance

      sendTxsAndThenPriorityTx(
        _ => sender.invokeScript(caller, thirdContract, Some("tikTok"), fee = invokeFee)._1.id,
        () => updateTikTok(typeName, priorityFee)
      ) { txs =>
        sender.balance(caller).balance shouldBe prevBalance - txs.size * invokeFee
        assertFailedTxs(txs)
      }
    }
  }

  test("invoke script error propagates failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    for (funcName <- Seq("transfer", "reissue", "burn")) {
      updateTikTok(funcName, setAssetScriptMinFee)
      updateAssetScript(result = true, smartAsset, thirdContract, setAssetScriptMinFee)

      val prevBalance = sender.balance(caller).balance

      sendTxsAndThenPriorityTx(
        _ => sender.invokeScript(caller, thirdContract, Some("tikTok"), fee = invokeFee)._1.id,
        () => updateAssetScript(result = false, smartAsset, thirdContract, priorityFee)
      ) { txs =>
        sender.balance(caller).balance shouldBe prevBalance - txs.size * invokeFee
        assertFailedTxs(txs)
      }
    }
  }

  test("sponsored fee on failed transaction should be charged correctly") {
    val invokeFee            = 0.005.waves + smartFee
    val invokeFeeInAsset     = invokeFee / 100000 // assetFee = feeInWaves / feeUnit * sponsorship
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAssetScript(result = true, smartAsset, thirdContract, setAssetScriptMinFee)
    updateTikTok("reissue", setAssetScriptMinFee)

    sender.sponsorAsset(thirdContract, sponsoredAsset, 1, sponsorFee + smartFee, waitForTx = true)
    sender.transfer(thirdContract, caller, assetAmount, smartMinFee, assetId = Some(sponsoredAsset), waitForTx = true)

    val prevBalance = sender.balance(thirdContract).balance

    sendTxsAndThenPriorityTx(
      _ => sender.invokeScript(caller, thirdContract, Some("tikTok"), fee = invokeFeeInAsset, feeAssetId = Some(sponsoredAsset))._1.id,
      () => updateAssetScript(result = false, smartAsset, thirdContract, priorityFee)
    ) { txs =>
      sender.assetBalance(caller, sponsoredAsset).balance shouldBe assetAmount - txs.size * invokeFeeInAsset
      sender.assetBalance(thirdContract, sponsoredAsset).balance shouldBe txs.size * invokeFeeInAsset
      sender.balance(thirdContract).balance shouldBe prevBalance - invokeFee * txs.size - priorityFee
      assertFailedTxs(txs)
    }
  }

  test("account state should not be changed after accepting failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    val initialEntries = List(
      IntegerDataEntry("n", -1),
      BooleanDataEntry("b", value = false),
      BinaryDataEntry("bn", ByteStr(Longs.toByteArray(-1))),
      StringDataEntry("s", "-1")
    )
    sender.broadcastData(pkByAddress(thirdContract), initialEntries, minFee + smartFee)
    updateAssetScript(result = true, smartAsset, thirdContract, setAssetScriptMinFee)

    sendTxsAndThenPriorityTx(
      i => sender.invokeScript(caller, thirdContract, Some("transferAndWrite"), args = List(Terms.CONST_LONG(i)), fee = invokeFee)._1.id,
      () => updateAssetScript(result = false, smartAsset, thirdContract, priorityFee)
    ) { txs =>
      val failed              = assertFailedTxs(txs)
      val lastSuccessEndArg   = txs.size - failed.size
      val lastSuccessStartArg = (lastSuccessEndArg - 3).max(1)

      val lastSuccessWrites =
        Range
          .inclusive(lastSuccessStartArg, lastSuccessEndArg)
          .map {
            case i if i % 4 == 0 => "n"  -> IntegerDataEntry("n", i)
            case i if i % 4 == 1 => "b"  -> BooleanDataEntry("b", i % 2 == 0)
            case i if i % 4 == 2 => "bn" -> BinaryDataEntry("bn", ByteStr(Longs.toByteArray(i)))
            case i if i % 4 == 3 => "s"  -> StringDataEntry("s", i.toString)
          }
          .toMap
      initialEntries.map(entry => entry.key -> entry).toMap.foreach {
        case (key, initial) =>
          sender.getDataByKey(thirdContract, key) shouldBe lastSuccessWrites.getOrElse(key, initial)
      }
      failed
    }
  }

  test("reject transactions if account script failed") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee * 2
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateTikTok("unknown", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, thirdContract, setAssetScriptMinFee)

    val prevBalance = sender.balance(caller).balance

    sendTxsAndThenPriorityTx(
      _ => sender.invokeScript(caller, thirdContract, Some("tikTok"), fee = invokeFee)._1.id,
      () =>
        sender
          .setScript(
            caller,
            Some(
              ScriptCompiler
                .compile(
                  s"""
                 |{-# STDLIB_VERSION 3 #-}
                 |{-# CONTENT_TYPE EXPRESSION #-}
                 |{-# SCRIPT_TYPE ACCOUNT #-}
                 |
                 |match (tx) {
                 |  case t: InvokeScriptTransaction => false
                 |  case _ => true
                 |}
                 |""".stripMargin,
                  ScriptEstimatorV3
                )
                .explicitGet()
                ._1
                .bytes()
                .base64
            ),
            fee = priorityFee,
            waitForTx = true
          )
          .id
    ) { txs =>
      val invalid = assertInvalidTxs(txs)
      sender.balance(caller).balance shouldBe prevBalance - (txs.size - invalid.size) * invokeFee - priorityFee
      invalid
    }
  }

  def updateTikTok(result: String, fee: Long): String =
    sender.broadcastData(pkByAddress(thirdContract), List(StringDataEntry("tikTok", result)), fee = fee, waitForTx = true).id

  override protected def waitForHeightArise(): Unit = nodes.waitForHeightArise()
}
