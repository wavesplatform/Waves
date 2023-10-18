package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAssetScript
import com.wavesplatform.api.http.DebugMessage
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.test.*
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration.*

class FailedTransactionSuite extends BaseTransactionSuite with CancelAfterFailure with FailedTransactionSuiteLike[String] with OverflowBlock {
  import FailedTransactionSuite.*
  import FailedTransactionSuiteLike.*
  import restApi.*

  private lazy val contract = sender.createKeyPair()
  private def caller        = thirdKeyPair

  private val assetAmount = 1000000000L
  private var smartAsset  = ""

  private def seller  = firstKeyPair
  private def buyer   = secondKeyPair
  private def matcher = thirdKeyPair

  private def sellerAddress  = firstKeyPair
  private def buyerAddress   = secondKeyPair
  private def matcherAddress = thirdKeyPair

  private lazy val contractAddress: String = contract.toAddress.toString

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    sender.transfer(sender.keyPair, contractAddress, 100.waves, minFee, waitForTx = true)

    smartAsset = sender
      .issue(
        contract,
        "Asset",
        "Description",
        assetAmount,
        8,
        script = Some(ScriptCompiler.compile("true", ScriptEstimatorV3(fixOverflow = true, overhead = false)).explicitGet()._1.bytes().base64),
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
         |func failAfterFirstCallHeight() = {
         |  strict c = ${"sigVerify(base58'', base58'', base58'') ||" * 16} true
         |  let heightEntry = match this.getInteger("heightEntry") {
         |    case _: Unit => height
         |    case h       => if (h == height) then h else throw("height differs")
         |  }
         |  [IntegerEntry("heightEntry", heightEntry)]
         |}
         |
        """.stripMargin

    val script = ScriptCompiler.compile(scriptTextV4, ScriptEstimatorV3(fixOverflow = true, overhead = false)).explicitGet()._1.bytes().base64
    sender.setScript(contract, Some(script), setScriptFee, waitForTx = true).id
  }

  test("InvokeScriptTransaction: reject transactions if account script failed") {
    val invokeFee            = 0.005.waves
    val setAssetScriptMinFee = setAssetScriptFee + smartFee
    val priorityFee          = setAssetScriptMinFee + invokeFee

    updateTikTok("unknown", setAssetScriptMinFee)
    updateAssetScript(result = true, smartAsset, contract, setAssetScriptMinFee)

    val prevBalance = sender.balance(caller.toAddress.toString).balance

    overflowBlock()
    sendTxsAndThenPriorityTx(
      _ => sender.invokeScript(caller, contractAddress, Some("tikTok"), fee = invokeFee)._1.id,
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
                     |  case _: InvokeScriptTransaction => false
                     |  case _ => true
                     |}
                     |""".stripMargin,
                  ScriptEstimatorV3(fixOverflow = true, overhead = false)
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
      sender.balance(caller.toAddress.toString).balance shouldBe prevBalance - (txs.size - invalid.size) * invokeFee - priorityFee
      invalid
    }
  }

  test("ExchangeTransaction: invalid exchange tx when asset script fails on broadcast") {
    val init = Seq(
      sender.setScript(firstKeyPair, None, setScriptFee + smartFee).id,
      sender.setScript(secondKeyPair, None, setScriptFee + smartFee).id,
      sender.setScript(thirdKeyPair, None, setScriptFee + smartFee).id
    )
    waitForTxs(init)

    val Precondition(amountAsset, priceAsset, buyFeeAsset, sellFeeAsset) =
      exchangePreconditions(
        Some(ScriptCompiler.compile("true", ScriptEstimatorV3(fixOverflow = true, overhead = false)).explicitGet()._1.bytes().base64)
      )

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
        sender.signedBroadcast(tx.json()),
        AssertiveApiError(TransactionNotAllowedByAssetScript.Id, "Transaction is not allowed by token-script")
      )
      assertInvalidTxs(Seq(tx.id().toString))
      updateAssetScript(result = true, invalidScriptAsset, owner, setAssetScriptFee + smartFee)
    }
  }

  test("InvokeScriptTransaction: revalidate transactions returned to UTXPool because of `min-micro-block-age`") {
    docker.restartNode(dockerNodes().head, configForMinMicroblockAge)

    val caller = sender.createKeyPair()
    sender.transfer(sender.keyPair, caller.toAddress.toString, 100.waves, minFee, waitForTx = true)

    val txs = (1 to 9).map { _ => sender.invokeScript(caller, contractAddress, Some("failAfterFirstCallHeight"), fee = invokeFee) }

    val failHeight = txs.map(tx => sender.waitForTransaction(tx._1.id)).map(_.height).max
    val failedTxs  = sender.blockAt(failHeight).transactions.map(_.id)

    assertFailedTxs(failedTxs)
  }

  def updateTikTok(result: String, fee: Long, waitForTx: Boolean = true): String =
    sender.broadcastData(contract, List(StringDataEntry("tikTok", result)), fee = fee, waitForTx = waitForTx).id

  private def waitForTxs(txs: Seq[String]): Unit =
    nodes.waitFor("preconditions", 500.millis)(_.transactionStatus(txs).forall(_.status == "confirmed"))(_.forall(identity))

  private def exchangePreconditions(initScript: Option[String]): Precondition = {
    val transfers = Seq(
      sender.transfer(sender.keyPair, sellerAddress.toAddress.toString, 100.waves).id,
      sender.transfer(sender.keyPair, buyerAddress.toAddress.toString, 100.waves).id,
      sender.transfer(sender.keyPair, matcherAddress.toAddress.toString, 100.waves).id
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

    val transferToSeller =
      sender.transfer(matcherAddress, sellerAddress.toAddress.toString, 1000000000, fee = minFee + smartFee, assetId = Some(sellFeeAsset)).id
    val transferToBuyer =
      sender.transfer(matcherAddress, buyerAddress.toAddress.toString, 1000000000, fee = minFee + smartFee, assetId = Some(buyFeeAsset)).id

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
