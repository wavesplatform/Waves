package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionStatus
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.it.{NTPTime, NodeConfigs}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import play.api.libs.json.JsObject

import scala.concurrent.duration._

class AcceptFailedScriptActivationSuite extends BaseTransactionSuite with NTPTime {
  import AcceptFailedScriptActivationSuite._

  private val (dApp, dAppKP)               = (firstAddress, pkByAddress(firstAddress))
  private val (caller, callerKP)           = (secondAddress, pkByAddress(secondAddress))
  private val (otherCaller, otherCallerKP) = (thirdAddress, pkByAddress(thirdAddress))

  private var asset = ""

  override def beforeAll(): Unit = {
    super.beforeAll()
    asset = sender.issue(dApp, "Asset", "Description", someAssetAmount, 8, script = assetScript(true), waitForTx = true).id

    val dAppScript = mkScript(
      s"""
         |{-# STDLIB_VERSION 3 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |
         |let asset = base58'$asset'
         |
         |@Callable(i)
         |func transfer() = {
         |  TransferSet([ScriptTransfer(i.caller, $minInvokeFee, asset)])
         |}
         |
         |@Callable(i)
         |func error() = {
         |  throw("Error in DApp")
         |}
         |
         |@Callable(i)
         |func write() = {
         |  WriteSet([DataEntry("a", "a")])
         |}
         |""".stripMargin
    )

    sender.setScript(dApp, dAppScript, setScriptFee, waitForTx = true).id
  }

  test("reject failed transaction before activation height") {
    val txs =
      (1 to MaxTxsInMicroBlock * 2).map { _ =>
        sender.invokeScript(caller, dApp, Some("transfer"), fee = minInvokeFee)._1.id
      }

    sender.setAssetScript(asset, dApp, priorityFee, assetScript(false), waitForTx = true)

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

    def check(): Unit = {
      val statuses = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.status) < txs.indexOf(s.status) }
      val failed   = statuses.dropWhile(s => s.status == "confirmed")

      failed.size should be > 0
      all(failed.map(_.status)) shouldBe "not_found"
      all(failed.map(_.applicationStatus)) shouldBe None
    }

    check() // liquid
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept valid transaction before activation height") {
    val tx = sender.invokeScript(caller, dApp, Some("write"), fee = invokeFee, waitForTx = true)._1.id

    def check(): Unit = {
      val txInfo = sender.transactionInfo[JsObject](tx)
      (txInfo \ "id").as[String] shouldBe tx
      txInfo.value.contains("applicationStatus") shouldBe false
    }

    check() // liquid
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept failed transaction after activation height") {
    sender.waitForHeight(ActivationHeight)

    sender.setAssetScript(asset, dApp, setAssetScriptFee + smartFee, assetScript(true), waitForTx = true)

    val txs =
      (1 to MaxTxsInMicroBlock * 2).map { _ =>
        sender.invokeScript(caller, dApp, Some("transfer"), fee = minInvokeFee)._1.id
      }

    sender.setAssetScript(asset, dApp, priorityFee, assetScript(false), waitForTx = true)

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

    def check(): Unit = {
      val statuses = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.status) < txs.indexOf(s.status) }
      all(statuses.map(_.status)) shouldBe "confirmed"
      all(statuses.map(_.applicationStatus.isDefined)) shouldBe true

      val failed = statuses.dropWhile(s => s.applicationStatus.contains("succeed"))

      failed.size should be > 0
      all(failed.flatMap(_.applicationStatus)) shouldBe "scriptExecutionFailed"

      statuses.foreach { s =>
        (sender.transactionInfo[JsObject](s.id) \ "applicationStatus").asOpt[String] shouldBe s.applicationStatus
      }
    }

    check() // liquid
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept valid transaction after activation height") {
    val tx = sender.invokeScript(caller, dApp, Some("write"), fee = invokeFee, waitForTx = true)._1.id

    def check(): Unit = {
      val txInfo = sender.transactionInfo[JsObject](tx)
      (txInfo \ "id").as[String] shouldBe tx
      (txInfo \ "applicationStatus").as[String] shouldBe "succeed"

      sender.transactionStatus(Seq(tx)).map(_.applicationStatus) shouldBe Seq(Some("succeed"))
    }

    check() // liquid
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept invalid by asset script InvokeScriptTransaction to utx and save it as failed after activation height") {
    sender.setAssetScript(asset, dApp, priorityFee, assetScript(false), waitForTx = true)

    val txs =
      (1 to MaxTxsInMicroBlock * 2).map { _ =>
        sender.invokeScript(caller, dApp, Some("transfer"), fee = minInvokeFee)._1.id
      }

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

    def check(): Unit = {
      val failed = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.status) < txs.indexOf(s.status) }
      failed.size shouldBe MaxTxsInMicroBlock * 2

      all(failed.map(_.status)) shouldBe "confirmed"
      all(failed.map(_.applicationStatus)) shouldBe 'defined
      all(failed.flatMap(_.applicationStatus)) shouldBe "scriptExecutionFailed"
    }

    check() // liquid
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept invalid by asset script in payment InvokeScriptTransaction to utx and save it as failed after activation height") {
    sender.setAssetScript(asset, dApp, priorityFee, assetScript(true), waitForTx = true)

    val callerBalance = sender.balance(caller).balance
    val callerAssetBalance = {
      val balance = sender.assetBalance(caller, asset).balance
      if (balance < MaxTxsInMicroBlock * 2) {
        sender.transfer(dApp, caller, MaxTxsInMicroBlock * 2 - balance, minFee + 2 * smartFee, Some(asset), waitForTx = true)
      }
      sender.assetBalance(caller, asset).balance
    }
    val dAppAssetBalance = sender.assetBalance(dApp, asset).balance

    sender.setAssetScript(asset, dApp, priorityFee, assetScript(false), waitForTx = true)

    val txs =
      (1 to MaxTxsInMicroBlock * 2).map { _ =>
        sender
          .invokeScript(
            caller,
            dApp,
            Some("write"),
            payment = Seq(InvokeScriptTransaction.Payment(2, IssuedAsset(ByteStr.decodeBase58(asset).get))),
            fee = minInvokeFee
          )
          ._1
          .id
      }

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

    def check(): Unit = {
      val failed = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.status) < txs.indexOf(s.status) }
      failed.size shouldBe MaxTxsInMicroBlock * 2

      all(failed.map(_.status)) shouldBe "confirmed"
      all(failed.map(_.applicationStatus)) shouldBe 'defined
      all(failed.flatMap(_.applicationStatus)) shouldBe "scriptExecutionFailed"

      sender.balance(caller).balance shouldBe callerBalance - MaxTxsInMicroBlock * 2 * minInvokeFee
      sender.assetBalance(caller, asset).balance shouldBe callerAssetBalance
      sender.assetBalance(dApp, asset).balance shouldBe dAppAssetBalance

      assertApiError(
        sender
          .invokeScript(
            caller,
            dApp,
            Some("write"),
            payment = Seq(InvokeScriptTransaction.Payment(callerAssetBalance + 1, IssuedAsset(ByteStr.decodeBase58(asset).get))),
            fee = minInvokeFee
          )
          ._1
          .id
      ) { e =>
        e.id shouldBe StateCheckFailed.Id
        e.statusCode shouldBe StateCheckFailed.Code.intValue
        e.message should include("Attempt to transfer unavailable funds")
      }
    }

    check() // liquid
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept invalid by DApp script InvokeScriptTransaction to utx and save it as failed after activation height") {
    sender.setAssetScript(asset, dApp, priorityFee, assetScript(true), waitForTx = true)

    val txs =
      (1 to MaxTxsInMicroBlock * 2).map { _ =>
        sender.invokeScript(caller, dApp, Some("error"), fee = minInvokeFee)._1.id
      }

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

    def check(): Unit = {
      val failed = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.status) < txs.indexOf(s.status) }
      failed.size shouldBe MaxTxsInMicroBlock * 2

      all(failed.map(_.status)) shouldBe "confirmed"
      all(failed.map(_.applicationStatus)) shouldBe 'defined
      all(failed.flatMap(_.applicationStatus)) shouldBe "scriptExecutionFailed"
    }

    check() // liquid
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("reject withdrawal of InvokeScriptTransaction fee from the funds received as a result of the script call execution") {
    sender.setAssetScript(asset, dApp, setAssetScriptFee + smartFee, assetScript(true), waitForTx = true)

    sender.transfer(otherCaller, caller, sender.balance(otherCaller).balance - minFee, fee = minFee, waitForTx = true)

    sender.balance(otherCaller).balance shouldBe 0L

    assertApiError(sender.invokeScript(otherCaller, dApp, Some("transfer"), fee = minInvokeFee)) { e =>
      e.id shouldBe StateCheckFailed.Id
      e.message should include("Accounts balance errors")
    }
  }

  test("reject withdrawal of matcher fee from orders in ExchangeTransaction") {
    sender.transfer(caller, otherCaller, issueFee, waitForTx = true)
    val tradeAsset = sender.issue(otherCaller, "Trade", decimals = 8: Byte, waitForTx = true).id

    sender.balance(otherCaller).balance shouldBe 0L

    val assetPair = AssetPair.createAssetPair("WAVES", tradeAsset)

    val ts = ntpTime.getTimestamp()
    val buyOrder = Order.buy(
      Order.V4,
      otherCallerKP,
      dAppKP.publicKey,
      assetPair.get,
      smartMatcherFee,
      100,
      ts,
      ts + Order.MaxLiveTime,
      smartMatcherFee
    )

    val sellOrder = Order.sell(
      Order.V4,
      callerKP,
      dAppKP.publicKey,
      assetPair.get,
      smartMatcherFee,
      100,
      ts,
      ts + Order.MaxLiveTime,
      smartMatcherFee
    )

    assertApiError(
      sender.broadcastExchange(
        dAppKP,
        sellOrder,
        buyOrder,
        amount = buyOrder.amount,
        price = buyOrder.price,
        buyMatcherFee = smartMatcherFee,
        sellMatcherFee = smartMatcherFee,
        fee = priorityFee,
        version = TxVersion.V3
      )
    ) { e =>
      e.id shouldBe StateCheckFailed.Id
      e.message should include("Accounts balance errors")
    }
  }

  test("accept invalid by order asset scripts ExchangeTransaction to utx and save it as failed after activation height") {
    sender.setAssetScript(asset, dApp, priorityFee, assetScript(true), waitForTx = true)
    sender.transfer(sender.address, dApp, 100.waves, waitForTx = true)
    val tradeAsset =
      sender
        .issue(dApp, "TradeAsset", quantity = someAssetAmount, decimals = 8: Byte, script = assetScript(true), fee = priorityFee, waitForTx = true)
        .id
    val feeAsset =
      sender
        .issue(dApp, "FeeAsset", quantity = someAssetAmount, decimals = 8: Byte, script = assetScript(true), fee = priorityFee, waitForTx = true)
        .id

    val preconditions = Seq(
      sender.transfer(dApp, caller, someAssetAmount / 3, assetId = Some(tradeAsset), fee = minFee + 2 * smartFee).id,
      sender.transfer(dApp, caller, someAssetAmount / 3, assetId = Some(feeAsset), fee = minFee + 2 * smartFee).id,
      sender.transfer(dApp, otherCaller, someAssetAmount / 3, assetId = Some(tradeAsset), fee = minFee + 2 * smartFee).id,
      sender.transfer(dApp, otherCaller, someAssetAmount / 3, assetId = Some(feeAsset), fee = minFee + 2 * smartFee).id
    )

    sender.waitFor("preconditions")(
      n => n.transactionStatus(preconditions),
      (statuses: Seq[TransactionStatus]) => statuses.forall(_.status == "confirmed"),
      100.millis
    )

    val assetPair = AssetPair.createAssetPair("WAVES", tradeAsset).get

    def orders: (Order, Order) = {
      val ts = ntpTime.getTimestamp()
      val buy = Order.buy(
        Order.V4,
        otherCallerKP,
        dAppKP.publicKey,
        assetPair,
        10,
        100,
        ts,
        ts + Order.MaxLiveTime,
        smartMatcherFee,
        matcherFeeAssetId = IssuedAsset(ByteStr.decodeBase58(feeAsset).get)
      )
      val sell =
        Order.sell(
          Order.V4,
          callerKP,
          dAppKP.publicKey,
          assetPair,
          10,
          100,
          ts,
          ts + Order.MaxLiveTime,
          smartMatcherFee,
          matcherFeeAssetId = IssuedAsset(ByteStr.decodeBase58(feeAsset).get)
        )
      (buy, sell)
    }

    {
      val (buy, sell) = orders
      sender.setAssetScript(tradeAsset, dApp, setAssetScriptFee + smartFee, assetScript(false), waitForTx = true)
      val tx = sender
        .broadcastExchange(
          dAppKP,
          buy,
          sell,
          buy.amount,
          buy.price,
          buy.matcherFee,
          sell.matcherFee,
          priorityFee,
          matcherFeeAssetId = Some(feeAsset),
          version = TxVersion.V3
        )
        .id

      val status = sender.waitFor("exchange accepted and saved as failed")(
        n => n.transactionStatus(Seq(tx)).head,
        (s: TransactionStatus) => s.status == "confirmed",
        100.millis
      )

      status.applicationStatus shouldBe Some("scriptExecutionFailed")
    }

    {
      val (buy, sell) = orders
      sender.setAssetScript(tradeAsset, dApp, setAssetScriptFee + smartFee, assetScript(true), waitForTx = true)
      sender.setAssetScript(feeAsset, dApp, setAssetScriptFee + smartFee, assetScript(false), waitForTx = true)
      val tx = sender
        .broadcastExchange(
          dAppKP,
          buy,
          sell,
          buy.amount,
          buy.price,
          buy.matcherFee,
          sell.matcherFee,
          priorityFee,
          matcherFeeAssetId = Some(feeAsset),
          version = TxVersion.V3
        )
        .id

      val status = sender.waitFor("exchange accepted and saved as failed")(
        n => n.transactionStatus(Seq(tx)).head,
        (s: TransactionStatus) => s.status == "confirmed",
        100.millis
      )

      status.applicationStatus shouldBe Some("scriptExecutionFailed")
    }

  }

  override protected def nodeConfigs: Seq[Config] = Configs
}

object AcceptFailedScriptActivationSuite {
  private val ActivationHeight   = 9
  private val UpdateInterval     = 3
  private val MaxTxsInMicroBlock = 2

  private val estimator = ScriptEstimatorV3

  private val priorityFee  = 5.waves
  private val minInvokeFee = invokeFee + smartFee // invoke fee + transfer action fee

  private def assetScript(result: Boolean): Option[String] =
    mkScript(
      s"""
       |match tx {
       |  case _: SetAssetScriptTransaction => true
       |  case _ => $result
       |}
       |""".stripMargin
    )

  private def mkScript(scriptText: String): Option[String] = Some(ScriptCompiler.compile(scriptText, estimator).explicitGet()._1.bytes().base64)

  private val Configs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.BlockV5.id, ActivationHeight)
        )
      )
      .overrideBase(_.raw(s"waves.blockchain.custom.functionality.min-asset-info-update-interval = $UpdateInterval"))
      .overrideBase(_.raw(s"waves.miner.max-transactions-in-micro-block = $MaxTxsInMicroBlock"))
      .buildNonConflicting()
}
