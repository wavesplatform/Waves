package com.wavesplatform.it.sync.transactions

import com.google.common.primitives.Longs
import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAccountScript
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.http.DebugMessage
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{DebugStateChanges, TransactionStatus}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration._
import scala.util.Try

class FailedTransactionSuite extends BaseTransactionSuite with CancelAfterFailure with FailedTransactionSuiteLike[String] with OverflowBlock {
  import FailedTransactionSuite._
  import FailedTransactionSuiteLike._
  import restApi._

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  private val contract = sender.createAddress()
  private val caller   = thirdAddress

  private val assetAmount    = 1000000000L
  private var smartAsset     = ""
  private var sponsoredAsset = ""

  private val seller         = acc0
  private val buyer          = acc1
  private val matcher        = acc2
  private val sellerAddress  = firstAddress
  private val buyerAddress   = secondAddress
  private val matcherAddress = thirdAddress

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sender.transfer(sender.address, contract, 100.waves, minFee, waitForTx = true)

    smartAsset = sender
      .issue(
        contract,
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
        contract,
        "Sponsored Asset",
        "Description",
        assetAmount,
        8,
        script = None,
        waitForTx = true
      )
      .id

    val scriptTextV4 =
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         |let asset = base58'$smartAsset'
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
         |@Callable(inv)
         |func canThrow() = {
         |  let action = valueOrElse(getString(this, "crash"), "no")
         |  if (action == "yes") then throw("Crashed by dApp")
         |  else []
         |}
         |
         |@Callable(inv)
         |func defineTxHeight(id: ByteVector) = [BooleanEntry(toBase58String(id), transactionHeightById(id).isDefined())]
         |
         |@Callable(inv)
         |func blockIsEven() = if height % 2 == 0 then [] else throw("block height is odd")
         |
        """.stripMargin

    val script = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    sender.setScript(contract, Some(script), setScriptFee, waitForTx = true).id
  }

  test("InvokeScriptTransaction: dApp error propagates failed transaction") {
    val invokeFee    = 0.005.waves
    val priorityData = List(StringDataEntry("crash", "yes"))
    val putDataFee   = calcDataFee(priorityData, 1)
    val priorityFee  = putDataFee + invokeFee

    val prevBalance = sender.balance(caller).balance

    sendPriorityTxAndThenOtherTxs(
      _ => sender.invokeScript(caller, contract, Some("canThrow"), fee = invokeFee)._1.id,
      () => sender.putData(contract, priorityData, priorityFee, waitForTx = true).id
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)
      val failed = assertFailedTxs(txs)

      sender.balance(caller).balance shouldBe prevBalance - txs.size * invokeFee

      failed.foreach { s =>
        checkStateChange(sender.debugStateChanges(s.id), 1, "Crashed by dApp")
      }

      failed
    }
  }

  test("InvokeScriptTransaction: insufficient action fees propagates failed transaction") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    for (typeName <- Seq("transfer", "issue", "reissue", "burn")) {
      updateTikTok("unknown", setAssetScriptMinFee)

      val prevBalance      = sender.balance(caller).balance
      val prevAssetBalance = sender.assetBalance(contract, smartAsset)
      val prevAssets       = sender.assetsBalance(contract)

      sendPriorityTxAndThenOtherTxs(
        _ => sender.invokeScript(caller, contract, Some("tikTok"), fee = invokeFee)._1.id,
        () => updateTikTok(typeName, priorityFee)
      ) { (txs, priorityTx) =>
        logPriorityTx(priorityTx)

        val failed = assertFailedTxs(txs)

        sender.balance(caller).balance shouldBe prevBalance - txs.size * invokeFee
        sender.assetBalance(contract, smartAsset) shouldBe prevAssetBalance
        sender.assetsBalance(contract).balances should contain theSameElementsAs prevAssets.balances

        val minFee        = if (typeName == "issue") invokeFee + issueFee else invokeFee + smartFee
        val scriptInvoked = if (typeName == "issue") 0 else 1
        val text = s"Fee in WAVES for InvokeScriptTransaction ($invokeFee in WAVES)" +
          s" with $scriptInvoked total scripts invoked does not exceed minimal value of $minFee WAVES."

        failed.foreach { s =>
          checkStateChange(sender.debugStateChanges(s.id), 2, text)
        }

        failed
      }
    }
  }

  test("InvokeScriptTransaction: invoke script error in action asset propagates failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateTikTok("reissue", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    val prevBalance      = sender.balance(caller).balance
    val prevAssetBalance = sender.assetBalance(contract, smartAsset)
    val prevAssets       = sender.assetsBalance(contract).balances.map(_.assetId)

    sendPriorityTxAndThenOtherTxs(
      _ => sender.invokeScript(caller, contract, Some("tikTok"), fee = invokeFee)._1.id,
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)

      val failed   = assertFailedTxs(txs)
      val reissued = 15 * (txs.size - failed.size)

      sender.balance(caller).balance shouldBe prevBalance - txs.size * invokeFee
      sender.assetBalance(contract, smartAsset) shouldBe prevAssetBalance.copy(balance = prevAssetBalance.balance + reissued)
      sender.assetsBalance(contract).balances.map(_.assetId) should contain theSameElementsAs prevAssets

      failed.foreach { s =>
        checkStateChange(sender.debugStateChanges(s.id), 3, "Transaction is not allowed by token-script")
      }

      failed
    }
  }

  test("InvokeScriptTransaction: invoke script error in payment asset propagates failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    val paymentAsset = sender
      .issue(
        caller,
        "paymentAsset",
        script = Some(ScriptCompiler.compile("true", ScriptEstimatorV3).explicitGet()._1.bytes().base64),
        fee = issueFee + smartFee,
        waitForTx = true
      )
      .id

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)
    updateTikTok("unknown", setAssetScriptMinFee)

    val prevBalance             = sender.balance(caller).balance
    val prevAssetBalance        = sender.assetBalance(contract, smartAsset)
    val prevPaymentAssetBalance = sender.assetBalance(caller, paymentAsset)
    val prevAssets              = sender.assetsBalance(contract).balances.map(_.assetId)

    sendPriorityTxAndThenOtherTxs(
      _ =>
        sender
          .invokeScript(
            caller,
            contract,
            Some("tikTok"),
            fee = invokeFee,
            payment = Seq(InvokeScriptTransaction.Payment(15, IssuedAsset(ByteStr.decodeBase58(paymentAsset).get)))
          )
          ._1
          .id,
      () => updateAssetScript(result = false, paymentAsset, caller, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)

      val failed = assertFailedTxs(txs)

      val succeedSize  = txs.size - failed.size
      val paymentDelta = -succeedSize * 15

      sender.balance(caller).balance shouldBe prevBalance - txs.size * invokeFee - priorityFee
      sender.assetBalance(contract, smartAsset) shouldBe prevAssetBalance

      val includePaymentAsset = if (txs.size > failed.size) List(paymentAsset) else List.empty
      sender
        .assetsBalance(contract)
        .balances
        .map(_.assetId) should contain theSameElementsAs prevAssets ++ includePaymentAsset
      sender.assetBalance(caller, paymentAsset) shouldBe prevPaymentAssetBalance.copy(balance = prevPaymentAssetBalance.balance + paymentDelta)

      failed.foreach { s =>
        checkStateChange(sender.debugStateChanges(s.id), 4, "Transaction is not allowed by token-script")
      }

      failed
    }
  }

  test("InvokeScriptTransaction: sponsored fee on failed transaction should be charged correctly") {
    val invokeFee            = 0.005.waves + smartFee
    val invokeFeeInAsset     = invokeFee / 100000 // assetFee = feeInWaves / feeUnit * sponsorship
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)
    updateTikTok("reissue", setAssetScriptMinFee)

    sender.sponsorAsset(contract, sponsoredAsset, 1, sponsorReducedFee + smartFee, waitForTx = true)
    sender.transfer(contract, caller, assetAmount, smartMinFee, assetId = Some(sponsoredAsset), waitForTx = true)

    val prevBalance = sender.balance(contract).balance

    sendPriorityTxAndThenOtherTxs(
      _ => sender.invokeScript(caller, contract, Some("tikTok"), fee = invokeFeeInAsset, feeAssetId = Some(sponsoredAsset))._1.id,
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)

      sender.assetBalance(caller, sponsoredAsset).balance shouldBe assetAmount - txs.size * invokeFeeInAsset
      sender.assetBalance(contract, sponsoredAsset).balance shouldBe txs.size * invokeFeeInAsset
      sender.balance(contract).balance shouldBe prevBalance - invokeFee * txs.size - priorityFee

      assertFailedTxs(txs)
    }
  }

  test("InvokeScriptTransaction: account state should not be changed after accepting failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    val initialEntries = List(
      IntegerDataEntry("n", -1),
      BooleanDataEntry("b", value = false),
      BinaryDataEntry("bn", ByteStr(Longs.toByteArray(-1))),
      StringDataEntry("s", "-1")
    )
    sender.broadcastData(pkByAddress(contract), initialEntries, minFee + smartFee, waitForTx = true)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    sendPriorityTxAndThenOtherTxs(
      i => sender.invokeScript(caller, contract, Some("transferAndWrite"), args = List(Terms.CONST_LONG(i)), fee = invokeFee)._1.id,
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)

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
          sender.getDataByKey(contract, key) shouldBe lastSuccessWrites.getOrElse(key, initial)
      }

      failed.foreach(s => checkStateChange(sender.debugStateChanges(s.id), 3, "Transaction is not allowed by token-script"))

      val failedIds             = failed.map(_.id).toSet
      val stateChangesByAddress = sender.debugStateChangesByAddress(contract, 10).takeWhile(sc => failedIds.contains(sc.id))
      stateChangesByAddress.size should be > 0
      stateChangesByAddress.foreach(info => checkStateChange(info, 3, "Transaction is not allowed by token-script"))

      failed
    }
  }

  test("InvokeScriptTransaction: reject transactions if account script failed") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateTikTok("unknown", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    val prevBalance = sender.balance(caller).balance

    overflowBlock()
    sendPriorityTxAndThenOtherTxs(
      _ => sender.invokeScript(caller, contract, Some("tikTok"), fee = invokeFee)._1.id,
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
            fee = priorityFee
          )
          .id
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)
      val invalid = assertInvalidTxs(txs)
      sender.balance(caller).balance shouldBe prevBalance - (txs.size - invalid.size) * invokeFee - priorityFee
      invalid
    }
  }

  test("InvokeScriptTransaction: transactionHeightById returns only succeed transactions") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateAccountScript(None, caller, setScriptFee + smartFee)
    updateTikTok("reissue", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    val failedTxs = sendPriorityTxAndThenOtherTxs(
      _ => sender.invokeScript(caller, contract, Some("tikTok"), fee = invokeFee)._1.id,
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)
      assertFailedTxs(txs)
    }

    checkTransactionHeightById(failedTxs)
  }

  test("ExchangeTransaction: failed exchange tx when asset script fails") {
    val init = Seq(
      sender.setScript(firstAddress, None, setScriptFee + smartFee).id,
      sender.setScript(secondAddress, None, setScriptFee + smartFee).id,
      sender.setScript(thirdAddress, None, setScriptFee + smartFee).id
    )
    waitForTxs(init)

    val Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset) =
      exchangePreconditions(Some(ScriptCompiler.compile("true", ScriptEstimatorV3).right.get._1.bytes().base64))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setAssetScriptFee + smartFee + fee * 10

    val allCases =
      Seq((amountAsset, sellerAddress), (priceAsset, buyerAddress), (sellFeeAsset, matcherAddress), (buyFeeAsset, matcherAddress))

    for ((invalidScriptAsset, owner) <- allCases) {
      val txsSend = (_: Int) => {
        val tx = mkExchange(buyer, seller, matcher, assetPair, fee, buyFeeAsset, sellFeeAsset, buyMatcherFee, sellMatcherFee)
        sender.signedBroadcast(tx.json()).id
      }

      sendPriorityTxAndThenOtherTxs(
        txsSend,
        () => updateAssetScript(result = false, invalidScriptAsset, owner, priorityFee)
      ) { (txs, priorityTx) =>
        logPriorityTx(priorityTx)
        assertFailedTxs(txs)
      }
      updateAssetScript(result = true, invalidScriptAsset, owner, setAssetScriptFee + smartFee)
    }
  }

  test("ExchangeTransaction: invalid exchange tx when account script fails") {
    val Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset) = exchangePreconditions(None)

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setScriptFee + smartFee + fee * 10

    val allCases = Seq(sellerAddress, buyerAddress, matcherAddress)
    allCases.foreach(address => updateAccountScript(None, address, setScriptFee + smartFee))

    for (invalidAccount <- allCases) {
      val txsSend = (_: Int) => {
        val tx = mkExchange(buyer, seller, matcher, assetPair, fee, buyFeeAsset, sellFeeAsset, buyMatcherFee, sellMatcherFee)
        sender.signedBroadcast(tx.json()).id
      }

      overflowBlock()
      sendPriorityTxAndThenOtherTxs(
        txsSend,
        () => updateAccountScript(Some(false), invalidAccount, priorityFee, waitForTx = false)
      ) { (txs, priorityTx) =>
        logPriorityTx(priorityTx)
        assertInvalidTxs(txs)
      }
      updateAccountScript(None, invalidAccount, setScriptFee + smartFee)
    }
  }

  test("ExchangeTransaction: transactionHeightById and transactionById returns only succeed transactions") {
    val Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset) =
      exchangePreconditions(Some(ScriptCompiler.compile("true", ScriptEstimatorV3).right.get._1.bytes().base64))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setAssetScriptFee + smartFee + fee * 10

    updateAssetScript(result = true, amountAsset, sellerAddress, priorityFee)

    val txsSend = (_: Int) => {
      val tx = mkExchange(buyer, seller, matcher, assetPair, fee, buyFeeAsset, sellFeeAsset, buyMatcherFee, sellMatcherFee)
      sender.signedBroadcast(tx.json()).id
    }

    val failedTxs = sendPriorityTxAndThenOtherTxs(
      txsSend,
      () => updateAssetScript(result = false, amountAsset, sellerAddress, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)
      assertFailedTxs(txs)
    }

    checkTransactionHeightById(failedTxs)

    val failedTxsSample = failedTxs.head

    sender.setScript(
      caller,
      Some(
        ScriptCompiler
          .compile(
            s"""
               |{-# STDLIB_VERSION 2 #-}
               |{-# CONTENT_TYPE EXPRESSION #-}
               |{-# SCRIPT_TYPE ACCOUNT #-}
               |
               |transactionById(fromBase58String("${failedTxsSample.id}")).isDefined()
               |""".stripMargin,
            ScriptEstimatorV3
          )
          .explicitGet()
          ._1
          .bytes()
          .base64
      ),
      fee = setScriptFee + smartFee,
      waitForTx = true
    )
    assertApiError(sender.transfer(caller, contract, 100, fee = smartMinFee)) { e =>
      e.message should include("Transaction is not allowed by account-script")
      e.id shouldBe TransactionNotAllowedByAccountScript.Id
    }
  }

  test("InvokeScriptTransaction: revalidate transactions returned to UTXPool because of `min-micro-block-age`") {
    docker.restartNode(dockerNodes().head, configForMinMicroblockAge)

    val caller = sender.createAddress()
    sender.transfer(sender.address, caller, 100.waves, minFee, waitForTx = true)

    val txs = collection.mutable.ListBuffer[String]()
    sender.waitFor("wait for even height")(n => n.height, (h: Int) => h % 2 == 0, 500.millis)
    sender.waitFor("send until odd height")({ n =>
      Try(n.invokeScript(caller, contract, Some("blockIsEven"), fee = invokeFee)._1.id).foreach(txs += _)
      n.height
    }, (h: Int) => h % 2 != 0, 5 second)

    miner.waitFor("empty utx")(_.utxSize, (_: Int) == 0, 1 second)
    assertFailedTxs(txs)
  }

  def updateTikTok(result: String, fee: Long): String =
    sender.broadcastData(pkByAddress(contract), List(StringDataEntry("tikTok", result)), fee = fee, waitForTx = true).id

  private def waitForTxs(txs: Seq[String]): Unit =
    nodes.waitFor("preconditions", 100.millis)(_.transactionStatus(txs).forall(_.status == "confirmed"))(
      statuses => statuses.forall(identity)
    )

  private def checkStateChange(info: DebugStateChanges, code: Int, text: String): Unit = {
    info.stateChanges shouldBe 'defined
    info.stateChanges.get.issues.size shouldBe 0
    info.stateChanges.get.reissues.size shouldBe 0
    info.stateChanges.get.burns.size shouldBe 0
    info.stateChanges.get.errorMessage shouldBe 'defined
    info.stateChanges.get.errorMessage.get.code shouldBe code
    info.stateChanges.get.errorMessage.get.text should include(text)
  }

  private def checkTransactionHeightById(failedTxs: Seq[TransactionStatus]): Unit = {
    val defineTxs = failedTxs.map { status =>
      sender
        .invokeScript(
          caller,
          contract,
          Some("defineTxHeight"),
          List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(status.id).get).explicitGet()),
          fee = invokeFee
        )
        ._1
        .id
    }

    waitForTxs(defineTxs)

    failedTxs.foreach(status => sender.getDataByKey(contract, status.id) shouldBe BooleanDataEntry(status.id, value = false))
  }

  private def exchangePreconditions(initScript: Option[String]): Precondition = {
    val transfers = Seq(
      sender.transfer(sender.address, sellerAddress, 100.waves).id,
      sender.transfer(sender.address, buyerAddress, 100.waves).id,
      sender.transfer(sender.address, matcherAddress, 100.waves).id
    )

    val amountAsset  = sender.issue(sellerAddress, "Amount asset", script = initScript, decimals = 8).id
    val priceAsset   = sender.issue(buyerAddress, "Price asset", script = initScript, decimals = 8).id
    val sellFeeAsset = sender.issue(matcherAddress, "Seller fee asset", script = initScript, decimals = 8).id
    val buyFeeAsset  = sender.issue(matcherAddress, "Buyer fee asset", script = initScript, decimals = 8).id

    val preconditions = transfers ++ Seq(
      amountAsset,
      priceAsset,
      sellFeeAsset,
      buyFeeAsset
    )

    waitForTxs(preconditions)

    val transferToSeller = sender.transfer(matcherAddress, sellerAddress, 1000000000, fee = minFee + smartFee, assetId = Some(sellFeeAsset)).id
    val transferToBuyer  = sender.transfer(matcherAddress, buyerAddress, 1000000000, fee = minFee + smartFee, assetId = Some(buyFeeAsset)).id

    waitForTxs(Seq(transferToSeller, transferToBuyer))

    Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset)
  }

  private def logPriorityTx(tx: String): Unit = {
    log.debug(s"Priority transaction: $tx")
    sender.printDebugMessage(DebugMessage(s"Priority transaction: $tx"))
  }

  override protected def waitForHeightArise(): Unit = nodes.waitForHeightArise()

  override protected def nodeConfigs: Seq[Config] = Configs
}

object FailedTransactionSuite {
  case class Precondition(amountAsset: String, priceAsset: String, buyFeeAsset: String, sellFeeAsset: String)
}
