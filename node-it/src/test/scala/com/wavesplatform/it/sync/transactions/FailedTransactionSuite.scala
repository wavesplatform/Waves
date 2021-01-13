package com.wavesplatform.it.sync.transactions

import com.google.common.primitives.Longs
import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.{ScriptExecutionError, TransactionNotAllowedByAccountScript, TransactionNotAllowedByAssetScript}
import com.wavesplatform.api.http.DebugMessage
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
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

import scala.concurrent.duration._

class FailedTransactionSuite extends BaseTransactionSuite with FailedTransactionSuiteLike[String] with OverflowBlock {

  import FailedTransactionSuite._
  import FailedTransactionSuiteLike._
  import restApi._

  private lazy val contract = miner.createKeyPair()

  private def caller = thirdKeyPair

  private val assetAmount = 1000000000L
  private var smartAsset = ""
  private var sponsoredAsset = ""

  private def seller  = firstKeyPair
  private def buyer   = secondKeyPair
  private def matcher = thirdKeyPair

  private def sellerAddress  = firstKeyPair
  private def buyerAddress   = secondKeyPair
  private def matcherAddress = thirdKeyPair

  private lazy val contractAddress: String = contract.toAddress.toString

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    miner.transfer(miner.keyPair, contractAddress, 100.waves, minFee, waitForTx = true)

    smartAsset = miner
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

    sponsoredAsset = miner
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
         |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} false
         |  let action = valueOrElse(getString(this, "tikTok"), "unknown")
         |  if (check) then []
         |  else if (action == "transfer") then [ScriptTransfer(inv.caller, 15, asset)]
         |  else if (action == "issue") then [Issue("new asset", "", 100, 8, true, unit, 0)]
         |  else if (action == "reissue") then [Reissue(asset, 15, true)]
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
         |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} true
         |
         |  if (action == "yes")
         |  then {
         |    if (check)
         |    then throw("Crashed by dApp")
         |    else throw("Crashed by dApp")
         |  }
         |  else []
         |}
         |
         |@Callable(inv)
         |func defineTxHeight(id: ByteVector) = [BooleanEntry(toBase58String(id), transactionHeightById(id).isDefined())]
         |
         |@Callable(inv)
         |func blockIsEven() =
         |  if (${"sigVerify(base58'', base58'', base58'') ||" * 16} height % 2 == 0)
         |  then []
         |  else throw("block height is odd")
         |
        """.stripMargin

    val script = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    miner.setScript(contract, Some(script), setScriptFee, waitForTx = true).id
  }

  test("InvokeScriptTransaction: dApp error propagates failed transaction") {
    val invokeFee    = 0.005.waves
    val priorityData = List(StringDataEntry("crash", "yes"))
    val putDataFee   = calcDataFee(priorityData, 1)
    val priorityFee  = putDataFee + invokeFee

    val prevBalance = miner.wavesBalance(caller.toAddress.toString)
    overflowBlock()
    sendTxsAndThenPriorityTx(
      _ => miner.invokeScript(caller, contractAddress, Some("canThrow"), fee = invokeFee)._1.id,
      () => miner.putData(contract, priorityData, priorityFee).id
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)
      waitForHeightArise()
      val failed = assertFailedTxs(txs)

      miner.wavesBalance(caller.toAddress.toString) shouldBe prevBalance - txs.size * invokeFee

      miner.transactionInfo[Seq[DebugStateChanges]](failed.map(_.id)).foreach { dsc =>
        checkStateChange(dsc, 1, "Crashed by dApp", strict = true)
      }

      assertApiError(
        miner.invokeScript(caller, contractAddress, Some("canThrow"), fee = invokeFee),
        AssertiveApiError(ScriptExecutionError.Id, "Error while executing account-script: Crashed by dApp")
      )

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

      val prevBalance      = miner.wavesBalance(caller.toAddress.toString)
      val prevAssetBalance = miner.assetBalance(contractAddress, smartAsset)
      val prevAssets       = miner.portfolio(contractAddress)

      overflowBlock()
      sendTxsAndThenPriorityTx(
        _ => miner.invokeScript(caller, contractAddress, Some("tikTok"), fee = invokeFee)._1.id,
        () => updateTikTok(typeName, priorityFee, waitForTx = false)
      ) { (txs, priorityTx) =>
        logPriorityTx(priorityTx)

        val failed = assertFailedTxs(txs)

        miner.wavesBalance(caller.toAddress.toString) shouldBe prevBalance - txs.size * invokeFee
        miner.assetBalance(contractAddress, smartAsset) shouldBe prevAssetBalance
        miner.portfolio(contractAddress).balances should contain theSameElementsAs prevAssets.balances

        val (scriptInvokedInfo, issuedInfo) =
          if (typeName == "issue")
            ("", " with 1 assets issued")
          else
            (" with 1 total scripts invoked", "")

        val minFee = if (typeName == "issue") invokeFee + issueFee else invokeFee + smartFee
        val text = s"Fee in WAVES for InvokeScriptTransaction ($invokeFee in WAVES)" +
          s"$scriptInvokedInfo$issuedInfo does not exceed minimal value of $minFee WAVES."

        miner.transactionInfo[Seq[DebugStateChanges]](failed.map(_.id)).foreach { dsc =>
          checkStateChange(dsc, 2, text)
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

    val prevBalance      = miner.wavesBalance(caller.toAddress.toString)
    val prevAssetBalance = miner.assetBalance(contractAddress, smartAsset)
    val prevAssets       = miner.portfolio(contractAddress).balances.map(_.assetId)

    sendTxsAndThenPriorityTx(
      _ => miner.invokeScript(caller, contractAddress, Some("tikTok"), fee = invokeFee)._1.id,
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)

      val failed   = assertFailedTxs(txs)
      val reissued = 15 * (txs.size - failed.size)

      miner.wavesBalance(caller.toAddress.toString) shouldBe prevBalance - txs.size * invokeFee
      miner.assetBalance(contractAddress, smartAsset) shouldBe prevAssetBalance.copy(balance = prevAssetBalance.balance + reissued)
      miner.portfolio(contractAddress).balances.map(_.assetId) should contain theSameElementsAs prevAssets

      failed.foreach { s =>
        checkStateChange(miner.debugStateChanges(s.id), 3, "Transaction is not allowed by script of the asset")
      }

      failed
    }
  }

  test("InvokeScriptTransaction: invoke script error in payment asset propagates failed transaction") {
    val invokeFee            = 0.005.waves + smartFee
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    val paymentAsset = miner
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

    val prevBalance             = miner.wavesBalance(caller.toAddress.toString)
    val prevAssetBalance        = miner.assetBalance(contractAddress, smartAsset)
    val prevPaymentAssetBalance = miner.assetBalance(caller.toAddress.toString, paymentAsset)
    val prevAssets              = miner.portfolio(contractAddress).balances.map(_.assetId)

    sendTxsAndThenPriorityTx(
      _ =>
        miner
          .invokeScript(
            caller,
            contractAddress,
            Some("tikTok"),
            fee = invokeFee,
            payment = Seq(InvokeScriptTransaction.Payment(15L, IssuedAsset(ByteStr.decodeBase58(paymentAsset).get)))
          )
          ._1
          .id,
      () => updateAssetScript(result = false, paymentAsset, caller, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)

      val failed = assertFailedTxs(txs)

      val succeedSize  = txs.size - failed.size
      val paymentDelta = -succeedSize * 15

      miner.wavesBalance(caller.toAddress.toString) shouldBe prevBalance - txs.size * invokeFee - priorityFee
      miner.assetBalance(contractAddress, smartAsset) shouldBe prevAssetBalance

      val includePaymentAsset = if (txs.size > failed.size) List(paymentAsset) else List.empty
      miner
        .portfolio(contractAddress)
        .balances
        .map(_.assetId) should contain theSameElementsAs prevAssets ++ includePaymentAsset
      miner.assetBalance(caller.toAddress.toString, paymentAsset) shouldBe prevPaymentAssetBalance.copy(
        balance = prevPaymentAssetBalance.balance + paymentDelta
      )

      failed.foreach { s =>
        checkStateChange(miner.debugStateChanges(s.id), 4, "Transaction is not allowed by script of the asset")
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

    miner.sponsorAsset(contract, sponsoredAsset, 1, sponsorReducedFee + smartFee, waitForTx = true)
    miner.transfer(contract, caller.toAddress.toString, assetAmount, smartMinFee, assetId = Some(sponsoredAsset), waitForTx = true)

    val prevBalance = miner.wavesBalance(contractAddress)
    sendTxsAndThenPriorityTx(
      _ => miner.invokeScript(caller, contractAddress, Some("tikTok"), fee = invokeFeeInAsset, feeAssetId = Some(sponsoredAsset))._1.id,
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)

      miner.assetBalance(caller.toAddress.toString, sponsoredAsset).balance shouldBe assetAmount - txs.size * invokeFeeInAsset
      miner.assetBalance(contractAddress, sponsoredAsset).balance shouldBe txs.size * invokeFeeInAsset
      miner.wavesBalance(contractAddress) shouldBe prevBalance - invokeFee * txs.size - priorityFee

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
    miner.broadcastData(contract, initialEntries, minFee + smartFee, waitForTx = true)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    sendTxsAndThenPriorityTx(
      i => miner.invokeScript(caller, contractAddress, Some("transferAndWrite"), args = List(Terms.CONST_LONG(i)), fee = invokeFee)._1.id,
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
          miner.getDataByKey(contractAddress, key) shouldBe lastSuccessWrites.getOrElse(key, initial)
      }

      failed.foreach(s => checkStateChange(miner.debugStateChanges(s.id), 3, "Transaction is not allowed by script of the asset"))

      val failedIds             = failed.map(_.id).toSet
      val stateChangesByAddress = miner.debugStateChangesByAddress(contractAddress, 10).takeWhile(sc => failedIds.contains(sc.id))
      stateChangesByAddress.size should be > 0
      stateChangesByAddress.foreach(info => checkStateChange(info, 3, "Transaction is not allowed by script of the asset"))

      failed
    }
  }

  test("InvokeScriptTransaction: reject transactions if account script failed") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateTikTok("unknown", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    val prevBalance = miner.wavesBalance(caller.toAddress.toString)
    overflowBlock()
    sendTxsAndThenPriorityTx(
      _ => miner.invokeScript(caller, contractAddress, Some("tikTok"), fee = invokeFee)._1.id,
      () =>
        miner
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
                     |  case _: InvokeScriptTransaction => false
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
      miner.wavesBalance(caller.toAddress.toString) shouldBe prevBalance - (txs.size - invalid.size) * invokeFee - priorityFee
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
    waitForEmptyUtx()
    overflowBlock()

    val failedTxs = sendTxsAndThenPriorityTx(
      _ => miner.invokeScript(caller, contractAddress, Some("tikTok"), fee = invokeFee)._1.id,
      () => updateAssetScript(result = false, smartAsset, contract, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)
      assertFailedTxs(txs)
    }

    checkTransactionHeightById(failedTxs)
  }

  test("ExchangeTransaction: transaction validates as failed when asset script fails") {
    val Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset) =
      exchangePreconditions(Some(ScriptCompiler.compile("true", ScriptEstimatorV3).explicitGet()._1.bytes().base64))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L

    val (assetScript, _) = ScriptCompiler.compile("if true then throw(\"error\") else false", ScriptEstimatorV3).explicitGet()
    val scriptTx         = miner.setAssetScript(priceAsset, buyerAddress, script = Some(assetScript.bytes().base64))
    nodes.waitForHeightAriseAndTxPresent(scriptTx.id)

    val tx     = mkExchange(buyer, seller, matcher, assetPair, fee, buyFeeAsset, sellFeeAsset, buyMatcherFee, sellMatcherFee)
    val result = miner.signedValidate(tx.json())
    (result \ "valid").as[Boolean] shouldBe false
    (result \ "error").as[String] should include("not allowed by script of the asset")
  }

  test("ExchangeTransaction: failed exchange tx when asset script fails") {
    val init = Seq(
      miner.setScript(firstKeyPair, None, setScriptFee + smartFee).id,
      miner.setScript(secondKeyPair, None, setScriptFee + smartFee).id,
      miner.setScript(thirdKeyPair, None, setScriptFee + smartFee).id
    )
    waitForTxs(init)

    val Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset) =
      exchangePreconditions(Some(ScriptCompiler.compile("true", ScriptEstimatorV3).explicitGet()._1.bytes().base64))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setAssetScriptFee + smartFee + fee * 10

    val allCases =
      Seq((amountAsset, sellerAddress), (priceAsset, buyerAddress), (sellFeeAsset, matcherAddress), (buyFeeAsset, matcherAddress))

    for ((invalidScriptAsset, owner) <- allCases) {
      overflowBlock()
      sendTxsAndThenPriorityTx(
        _ =>
          miner
            .signedBroadcast(mkExchange(buyer, seller, matcher, assetPair, fee, buyFeeAsset, sellFeeAsset, buyMatcherFee, sellMatcherFee).json())
            .id,
        () => updateAssetScript(result = false, invalidScriptAsset, owner, priorityFee, waitForTx = false)
      ) { (txs, priorityTx) =>
        logPriorityTx(priorityTx)
        assertFailedTxs(txs)
      }
      updateAssetScript(result = true, invalidScriptAsset, owner, setAssetScriptFee + smartFee)
    }
  }

  test("ExchangeTransaction: invalid exchange tx when asset script fails on broadcast") {
    val init = Seq(
      miner.setScript(firstKeyPair, None, setScriptFee + smartFee).id,
      miner.setScript(secondKeyPair, None, setScriptFee + smartFee).id,
      miner.setScript(thirdKeyPair, None, setScriptFee + smartFee).id
    )
    waitForTxs(init)

    val Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset) =
      exchangePreconditions(Some(ScriptCompiler.compile("true", ScriptEstimatorV3).explicitGet()._1.bytes().base64))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L

    val allCases =
      Seq((amountAsset, sellerAddress), (priceAsset, buyerAddress), (sellFeeAsset, matcherAddress), (buyFeeAsset, matcherAddress))

    for ((invalidScriptAsset, owner) <- allCases) {
      updateAssetScript(result = false, invalidScriptAsset, owner, setAssetScriptFee + smartFee)
      val tx = mkExchange(buyer, seller, matcher, assetPair, fee, buyFeeAsset, sellFeeAsset, buyMatcherFee, sellMatcherFee)
      assertApiError(
        miner.signedBroadcast(tx.json()),
        AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
      )
      assertInvalidTxs(Seq(tx.id().toString))
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
        miner.signedBroadcast(tx.json()).id
      }

      updateAccountScript(None, invalidAccount, setScriptFee + smartFee)
      overflowBlock()
      sendTxsAndThenPriorityTx(
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
      exchangePreconditions(Some(ScriptCompiler.compile("true", ScriptEstimatorV3).explicitGet()._1.bytes().base64))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setAssetScriptFee + smartFee + fee * 10

    updateAssetScript(result = true, amountAsset, sellerAddress, priorityFee)

    val txsSend = (_: Int) => {
      val tx = mkExchange(buyer, seller, matcher, assetPair, fee, buyFeeAsset, sellFeeAsset, buyMatcherFee, sellMatcherFee)
      miner.signedBroadcast(tx.json()).id
    }

    val failedTxs = sendTxsAndThenPriorityTx(
      txsSend,
      () => updateAssetScript(result = false, amountAsset, sellerAddress, priorityFee)
    ) { (txs, priorityTx) =>
      logPriorityTx(priorityTx)
      assertFailedTxs(txs)
    }

    checkTransactionHeightById(failedTxs)

    val failedTxsSample = failedTxs.head

    miner.setScript(
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
    assertApiError(miner.transfer(caller, contractAddress, 100, fee = smartMinFee)) { e =>
      e.message should include("Transaction is not allowed by account-script")
      e.id shouldBe TransactionNotAllowedByAccountScript.Id
    }
  }

  test("InvokeScriptTransaction: revalidate transactions returned to UTXPool because of `min-micro-block-age`") {
    docker.restartNode(dockerNodes().head, configForMinMicroblockAge)

    val caller = miner.createKeyPair()
    miner.transfer(miner.keyPair, caller.toAddress.toString, 100.waves, minFee, waitForTx = true)

    miner.waitFor("even height")(n => n.height, (h: Int) => h % 2 == 0, 500.millis)

    var ids = Set.empty[String]
    while (miner.height % 2 == 0) {
      val tx = miner.invokeScript(caller, contractAddress, Some("blockIsEven"), fee = invokeFee, waitForTx = true)._1
      ids += tx.id
    }

    val height = miner.waitFor("odd height")(n => n.height, (h: Int) => h % 2 != 0, 500.millis)
    nodes.waitForHeightArise()
    val blockTxs = miner.blockAt(height).transactions.map(_.id).filter(ids)
    assertFailedTxs(blockTxs)
  }

  def updateTikTok(result: String, fee: Long, waitForTx: Boolean = true): String =
    miner.broadcastData(contract, List(StringDataEntry("tikTok", result)), fee = fee, waitForTx = waitForTx).id

  private def waitForTxs(txs: Seq[String]): Unit =
    nodes.waitFor("preconditions", 500.millis)(_.transactionStatus(txs).forall(_.status == "confirmed"))(
      statuses => statuses.forall(identity)
    )

  private def checkStateChange(info: DebugStateChanges, code: Int, text: String, strict: Boolean = false): Unit = {
    info.stateChanges shouldBe defined
    info.stateChanges.get.issues.size shouldBe 0
    info.stateChanges.get.reissues.size shouldBe 0
    info.stateChanges.get.burns.size shouldBe 0
    info.stateChanges.get.error shouldBe defined
    info.stateChanges.get.error.get.code shouldBe code
    if (strict)
      info.stateChanges.get.error.get.text shouldBe text
    else
      info.stateChanges.get.error.get.text should include(text)
  }

  private def checkTransactionHeightById(failedTxs: Seq[TransactionStatus]): Unit = {
    val defineTxs = failedTxs.map { status =>
      miner
        .invokeScript(
          caller,
          contractAddress,
          Some("defineTxHeight"),
          List(Terms.CONST_BYTESTR(ByteStr.decodeBase58(status.id).get).explicitGet()),
          fee = invokeFee
        )
        ._1
        .id
    }

    waitForTxs(defineTxs)

    failedTxs.foreach(status => miner.getDataByKey(contractAddress, status.id) shouldBe BooleanDataEntry(status.id, value = false))
  }

  private def exchangePreconditions(initScript: Option[String]): Precondition = {
    val transfers = Seq(
      miner.transfer(miner.keyPair, sellerAddress.toAddress.toString, 100.waves).id,
      miner.transfer(miner.keyPair, buyerAddress.toAddress.toString, 100.waves).id,
      miner.transfer(miner.keyPair, matcherAddress.toAddress.toString, 100.waves).id
    )

    val amountAsset  = miner.issue(sellerAddress, "Amount asset", script = initScript, decimals = 8).id
    val priceAsset   = miner.issue(buyerAddress, "Price asset", script = initScript, decimals = 8).id
    val sellFeeAsset = miner.issue(matcherAddress, "Seller fee asset", script = initScript, decimals = 8).id
    val buyFeeAsset  = miner.issue(matcherAddress, "Buyer fee asset", script = initScript, decimals = 8).id

    val preconditions = transfers ++ Seq(
      amountAsset,
      priceAsset,
      sellFeeAsset,
      buyFeeAsset
    )

    waitForTxs(preconditions)

    val transferToSeller =
      miner.transfer(matcherAddress, sellerAddress.toAddress.toString, 1000000000, fee = minFee + smartFee, assetId = Some(sellFeeAsset)).id
    val transferToBuyer =
      miner.transfer(matcherAddress, buyerAddress.toAddress.toString, 1000000000, fee = minFee + smartFee, assetId = Some(buyFeeAsset)).id

    waitForTxs(Seq(transferToSeller, transferToBuyer))

    Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset)
  }

  private def logPriorityTx(tx: String): Unit = {
    log.debug(s"Priority transaction: $tx")
    miner.printDebugMessage(DebugMessage(s"Priority transaction: $tx"))
  }

  override protected def waitForHeightArise(): Unit = nodes.waitForHeightArise()

  override protected def nodeConfigs: Seq[Config] = Configs
}

object FailedTransactionSuite {
  case class Precondition(amountAsset: String, priceAsset: String, buyFeeAsset: String, sellFeeAsset: String)
}
