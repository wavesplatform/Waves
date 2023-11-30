package com.wavesplatform.it.sync.activation

import scala.concurrent.duration.*

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.{NodeConfigs, NTPTime}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.TransactionStatus
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.sync.transactions.OverflowBlock
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{TxExchangePrice, TxVersion}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import play.api.libs.json.JsObject

class AcceptFailedScriptActivationSuite extends BaseTransactionSuite with NTPTime with OverflowBlock {
  import AcceptFailedScriptActivationSuite.*

  private lazy val (dApp, dAppKP)               = (firstAddress, firstKeyPair)
  private lazy val (caller, callerKP)           = (secondAddress, secondKeyPair)
  private lazy val (otherCaller, otherCallerKP) = (thirdAddress, thirdKeyPair)

  private var asset = ""

  override def beforeAll(): Unit = {
    super.beforeAll()
    asset = sender.issue(dAppKP, "Asset", "Description", someAssetAmount, 8, script = assetScript(true), waitForTx = true).id

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
         |  let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} true
         |  if (check)
         |    then throw("Error in DApp")
         |    else throw("Error in DApp")
         |}
         |
         |@Callable(i)
         |func write() = {
         |  WriteSet([DataEntry("a", "a")])
         |}
         |""".stripMargin
    )

    sender.setScript(dAppKP, dAppScript, setScriptFee, waitForTx = true).id
    nodes.waitForEmptyUtx()
    nodes.waitForHeightArise()
  }

  test("reject failed transaction before activation height") {
    overflowBlock()
    sender.waitForHeight(
      sender
        .waitForTransaction(sender.setAssetScript(asset, dAppKP, priorityFee, assetScript(false)).id)
        .height + 1
    )

    assertApiErrorRaised(sender.invokeScript(callerKP, dApp, Some("transfer"), fee = minInvokeFee))
  }

  test("accept valid transaction before activation height") {
    val tx = sender.invokeScript(callerKP, dApp, Some("write"), fee = invokeFee, waitForTx = true)._1.id
    all(sender.lastBlock().transactions.map(_.applicationStatus)) shouldBe None

    def check(): Unit = {
      val txInfo   = sender.transactionInfo[JsObject](tx)
      val txHeight = (txInfo \ "height").as[Int]
      (txInfo \ "id").as[String] shouldBe tx
      txInfo.value.contains("applicationStatus") shouldBe false
      val block = sender.blockAt(txHeight)
      all(block.transactions.map(_.applicationStatus)) shouldBe None
      all(sender.blockById(block.id).transactions.map(_.applicationStatus)) shouldBe None
      all(sender.blockSeq(txHeight - 1, txHeight).flatMap(_.transactions.map(_.applicationStatus))) shouldBe None
      sender.stateChanges(tx).applicationStatus shouldBe None
      all(sender.transactionsByAddress(caller, 1).map(_.applicationStatus)) shouldBe None
    }

    nodes.waitForHeightArise()
    check() // hardened
    all(sender.blockSeqByAddress(sender.address, 1, sender.height).flatMap(_.transactions.map(_.applicationStatus))) shouldBe None
  }

  test("accept failed transaction after activation height") {
    docker.restartNode(dockerNodes().head, configs(activate = true).head)
    val startHeight = sender.height

    sender.setAssetScript(asset, dAppKP, setAssetScriptFee + smartFee, assetScript(true), waitForTx = true)
    overflowBlock()
    sender.setAssetScript(asset, dAppKP, priorityFee, assetScript(false))
    val txs =
      (1 to MaxTxsInMicroBlock * 2).map { _ =>
        sender.invokeScript(callerKP, dApp, Some("transfer"), fee = minInvokeFee)._1.id
      }

    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

    all(sender.lastBlock().transactions.map(_.applicationStatus.isDefined)) shouldBe true

    def check(): Unit = {
      val statuses = sender.transactionStatus(txs)
      all(statuses.map(_.status)) shouldBe "confirmed"
      all(statuses.map(_.applicationStatus.isDefined)) shouldBe true

      val failed = statuses.filterNot(s => s.applicationStatus.contains("succeeded"))

      failed.size should be > 0
      all(failed.flatMap(_.applicationStatus)) shouldBe "script_execution_failed"

      statuses.foreach { s =>
        (sender.transactionInfo[JsObject](s.id) \ "applicationStatus").asOpt[String] shouldBe s.applicationStatus
      }

      val heightToId            = statuses.map(s => s.height.get -> s.id).toMap
      val idToApplicationStatus = statuses.map(s => s.id -> s.applicationStatus).toMap
      heightToId.keys.foreach { h =>
        val block = sender.blockAt(h)
        block.transactions.foreach { tx =>
          tx.applicationStatus shouldBe idToApplicationStatus.getOrElse(tx.id, Some("succeeded"))
          if (tx._type == InvokeScriptTransaction.typeId) {
            sender.stateChanges(tx.id).applicationStatus shouldBe idToApplicationStatus.getOrElse(tx.id, Some("succeeded"))
          }
        }
        sender.blockById(block.id).transactions.foreach { tx =>
          tx.applicationStatus shouldBe idToApplicationStatus.getOrElse(tx.id, Some("succeeded"))
        }

        sender.blockSeq(h - 1, h).flatMap(_.transactions).foreach { tx =>
          tx.applicationStatus shouldBe idToApplicationStatus.getOrElse(tx.id, Some("succeeded"))
        }
      }
      sender.transactionsByAddress(caller, txs.size).foreach { s =>
        s.applicationStatus shouldBe idToApplicationStatus.getOrElse(s.id, Some("succeeded"))
      }
      sender.blockSeqByAddress(sender.address, startHeight, sender.height).flatMap(_.transactions).foreach { tx =>
        tx.applicationStatus shouldBe idToApplicationStatus.getOrElse(tx.id, Some("succeeded"))
      }
    }

    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept valid transaction after activation height") {
    val tx = sender.invokeScript(callerKP, dApp, Some("write"), fee = invokeFee, waitForTx = true)._1.id

    def check(): Unit = {
      val txInfo = sender.transactionInfo[JsObject](tx)
      (txInfo \ "id").as[String] shouldBe tx
      (txInfo \ "applicationStatus").as[String] shouldBe "succeeded"

      sender.transactionStatus(Seq(tx)).map(_.applicationStatus) shouldBe Seq(Some("succeeded"))
    }

    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept invalid by asset script InvokeScriptTransaction to utx and save it as failed after activation height") {
    sender.setAssetScript(asset, dAppKP, priorityFee, assetScript(true), waitForTx = true)

    overflowBlock()

    val txs =
      (1 to MaxTxsInMicroBlock * 2).map { i =>
        sender.invokeScript(callerKP, dApp, Some("transfer"), fee = minInvokeFee + i)._1.id
      }

    sender.setAssetScript(asset, dAppKP, priorityFee, assetScript(false))

    def check(): Unit = {
      val failed = sender.transactionStatus(txs).filterNot(_.applicationStatus.contains("succeeded"))
      failed should not be empty

      all(failed.map(_.status)) shouldBe "confirmed"
      all(failed.map(_.applicationStatus)) shouldBe Some("script_execution_failed")
    }

    sender.waitFor("empty utx")(n => n.utxSize, (_: Int) == 0, 100.millis)
    nodes.waitForHeightArise()
    check() // hardened
  }

  test("accept invalid by asset script in payment InvokeScriptTransaction to utx and save it as failed after activation height") {
    sender.setAssetScript(asset, dAppKP, priorityFee, assetScript(true), waitForTx = true)

    val invokesCount  = MaxTxsInMicroBlock * 2
    val callerBalance = sender.balance(caller).balance
    val callerAssetBalance = {
      val balance = sender.assetBalance(caller, asset).balance
      if (balance < invokesCount) {
        sender.transfer(dAppKP, caller, invokesCount - balance, minFee + 2 * smartFee, Some(asset), waitForTx = true)
      }
      sender.assetBalance(caller, asset).balance
    }
    val dAppAssetBalance = sender.assetBalance(dApp, asset).balance

    nodes.waitFor("empty utx")(_.utxSize)(_.forall(_ == 0))
    nodes.waitForHeightArise()
    overflowBlock()

    val txs =
      (1 to invokesCount).map { _ =>
        sender
          .invokeScript(
            callerKP,
            dApp,
            Some("write"),
            payment = Seq(InvokeScriptTransaction.Payment(1L, IssuedAsset(ByteStr.decodeBase58(asset).get))),
            fee = minInvokeFee
          )
          ._1
          .id
      }

    sender.setAssetScript(asset, dAppKP, priorityFee, assetScript(false))
    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

    def check(): Unit = {
      val failed = sender.transactionStatus(txs).filterNot(_.applicationStatus.contains("succeeded"))
      failed should not be empty

      all(failed.map(_.status)) shouldBe "confirmed"
      all(failed.map(_.applicationStatus)) shouldBe Some("script_execution_failed")

      sender.balance(caller).balance shouldBe callerBalance - invokesCount * minInvokeFee
      sender.assetBalance(caller, asset).balance should be > 0L
      sender.assetBalance(dApp, asset).balance shouldBe dAppAssetBalance +- invokesCount

      assertApiError(
        sender
          .invokeScript(
            callerKP,
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

    nodes.waitForHeightArise()
    check() // hardened
  }

  test("reject withdrawal of InvokeScriptTransaction fee from the funds received as a result of the script call execution") {
    sender.setAssetScript(asset, dAppKP, setAssetScriptFee + smartFee, assetScript(true), waitForTx = true)

    sender.transfer(otherCallerKP, caller, sender.balance(otherCaller).balance - minFee, fee = minFee, waitForTx = true)

    sender.balance(otherCaller).balance shouldBe 0L

    assertApiError(sender.invokeScript(otherCallerKP, dApp, Some("transfer"), fee = minInvokeFee)) { e =>
      e.id shouldBe StateCheckFailed.Id
      e.message should include("Accounts balance errors")
    }
  }

  test("reject withdrawal of matcher fee from orders in ExchangeTransaction") {
    sender.transfer(callerKP, otherCaller, issueFee, waitForTx = true)
    val tradeAsset = sender.issue(otherCallerKP, "Trade", decimals = 8: Byte, waitForTx = true).id

    sender.balance(otherCaller).balance shouldBe 0L

    val assetPair = AssetPair.createAssetPair("WAVES", tradeAsset)

    val ts = ntpTime.getTimestamp()
    val buyOrder = Order
      .buy(
        Order.V4,
        otherCallerKP,
        dAppKP.publicKey,
        assetPair.get,
        smartMatcherFee,
        100L,
        ts,
        ts + 2.days.toMillis,
        smartMatcherFee
      )
      .explicitGet()

    val sellOrder = Order
      .sell(
        Order.V4,
        callerKP,
        dAppKP.publicKey,
        assetPair.get,
        smartMatcherFee,
        100L,
        ts,
        ts + 2.days.toMillis,
        smartMatcherFee
      )
      .explicitGet()

    assertApiError(
      sender.broadcastExchange(
        dAppKP,
        sellOrder,
        buyOrder,
        amount = buyOrder.amount,
        price = TxExchangePrice.unsafeFrom(buyOrder.price.value),
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
    sender.setAssetScript(asset, dAppKP, priorityFee, assetScript(true), waitForTx = true)
    sender.transfer(sender.keyPair, dApp, 100.waves, waitForTx = true)
    val tradeAsset =
      sender
        .issue(dAppKP, "TradeAsset", quantity = someAssetAmount, decimals = 8: Byte, script = assetScript(true), fee = priorityFee, waitForTx = true)
        .id
    val feeAsset =
      sender
        .issue(dAppKP, "FeeAsset", quantity = someAssetAmount, decimals = 8: Byte, script = assetScript(true), fee = priorityFee, waitForTx = true)
        .id

    val preconditions = Seq(
      sender.transfer(dAppKP, caller, someAssetAmount / 3, assetId = Some(tradeAsset), fee = minFee + 2 * smartFee).id,
      sender.transfer(dAppKP, caller, someAssetAmount / 3, assetId = Some(feeAsset), fee = minFee + 2 * smartFee).id,
      sender.transfer(dAppKP, otherCaller, someAssetAmount / 3, assetId = Some(tradeAsset), fee = minFee + 2 * smartFee).id,
      sender.transfer(dAppKP, otherCaller, someAssetAmount / 3, assetId = Some(feeAsset), fee = minFee + 2 * smartFee).id
    )

    sender.waitFor("preconditions")(
      n => n.transactionStatus(preconditions),
      (statuses: Seq[TransactionStatus]) => statuses.forall(_.status == "confirmed"),
      100.millis
    )

    val assetPair = AssetPair.createAssetPair("WAVES", tradeAsset).get

    def orders: (Order, Order) = {
      val ts = ntpTime.getTimestamp()
      val buy = Order
        .buy(
          Order.V4,
          otherCallerKP,
          dAppKP.publicKey,
          assetPair,
          10L,
          100L,
          ts,
          ts + 2.days.toMillis,
          smartMatcherFee,
          matcherFeeAssetId = IssuedAsset(ByteStr.decodeBase58(feeAsset).get)
        )
        .explicitGet()
      val sell =
        Order
          .sell(
            Order.V4,
            callerKP,
            dAppKP.publicKey,
            assetPair,
            10L,
            100L,
            ts,
            ts + 2.days.toMillis,
            smartMatcherFee,
            matcherFeeAssetId = IssuedAsset(ByteStr.decodeBase58(feeAsset).get)
          )
          .explicitGet()
      (buy, sell)
    }

    {
      val (buy, sell) = orders
      overflowBlock()

      sender.setAssetScript(tradeAsset, dAppKP, priorityFee, assetScript(false))
      val tx = sender
        .broadcastExchange(
          dAppKP,
          buy,
          sell,
          buy.amount,
          TxExchangePrice.unsafeFrom(buy.price.value),
          buy.matcherFee.value,
          sell.matcherFee.value,
          matcherFee + smartFee * 3,
          version = TxVersion.V3
        )
        .id

      val status = sender.waitFor("exchange accepted and saved as failed")(
        n => n.transactionStatus(Seq(tx)).head,
        (s: TransactionStatus) => s.status == "confirmed",
        100.millis
      )

      status.applicationStatus shouldBe Some("script_execution_failed")
    }

    {
      val (buy, sell) = orders
      sender.setAssetScript(tradeAsset, dAppKP, setAssetScriptFee + smartFee, assetScript(true), waitForTx = true)

      overflowBlock()
      sender.setAssetScript(feeAsset, dAppKP, setAssetScriptFee + smartFee, assetScript(false))
      val tx = sender
        .broadcastExchange(
          dAppKP,
          buy,
          sell,
          buy.amount,
          TxExchangePrice.unsafeFrom(buy.price.value),
          buy.matcherFee.value,
          sell.matcherFee.value,
          matcherFee + smartFee * 3,
          version = TxVersion.V3
        )
        .id

      val status = sender.waitFor("exchange accepted and saved as failed")(
        n => n.transactionStatus(Seq(tx)).head,
        (s: TransactionStatus) => s.status == "confirmed",
        100.millis
      )

      status.applicationStatus shouldBe Some("script_execution_failed")
    }

  }

  override protected def nodeConfigs: Seq[Config] = configs(activate = false)
}

object AcceptFailedScriptActivationSuite {
  private val UpdateInterval     = 3
  private val MaxTxsInMicroBlock = 2

  private val estimator = ScriptEstimatorV3.latest

  private val priorityFee  = 5.waves
  private val minInvokeFee = invokeFee + smartFee // invoke fee + transfer action fee

  private def assetScript(result: Boolean): Option[String] =
    mkScript(
      s"""
         |match tx {
         |  case _: SetAssetScriptTransaction => true
         |  case _ =>
         |   let check = ${"sigVerify(base58'', base58'', base58'') ||" * 16} false
         |   if (check) then false else $result
         |}
         |""".stripMargin
    )

  private def mkScript(scriptText: String): Option[String] = Some(ScriptCompiler.compile(scriptText, estimator).explicitGet()._1.bytes().base64)

  private def configs(activate: Boolean): Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.BlockV5.id, if (activate) 0 else 9999)
        )
      )
      .overrideBase(_.raw(s"waves.blockchain.custom.functionality.min-asset-info-update-interval = $UpdateInterval"))
      .overrideBase(_.raw(s"waves.miner.max-transactions-in-micro-block = $MaxTxsInMicroBlock"))
      .buildNonConflicting()
}
