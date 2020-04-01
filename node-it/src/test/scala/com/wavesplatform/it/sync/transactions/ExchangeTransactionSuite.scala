package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.http.DebugMessage
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.exchangeTx
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.it.{NTPTime, NodeConfigs}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{Asset, TxVersion}
import com.wavesplatform.utils._
import play.api.libs.json.{JsNumber, JsObject, JsString, Json}

import scala.concurrent.duration._

class ExchangeTransactionSuite extends BaseTransactionSuite with NTPTime {
  var exchAsset: IssueTransaction = IssueTransaction(
    TxVersion.V1,
    sender = sender.privateKey,
    "myasset".utf8Bytes,
    "my asset description".utf8Bytes,
    quantity = someAssetAmount,
    decimals = 2,
    reissuable = true,
    script = None,
    fee = 1.waves,
    timestamp = System.currentTimeMillis()
  ).signWith(sender.privateKey)

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  private val transactionV1versions = (1: Byte, 1: Byte, 1: Byte) // in ExchangeTransactionV1 only orders V1 are supported
  private val transactionV2versions = for {
    o1ver <- 1 to 3
    o2ver <- 1 to 3
    txVer <- 2 to 3
  } yield (o1ver.toByte, o2ver.toByte, txVer.toByte)

  private val versions = transactionV1versions +: transactionV2versions

  test("cannot exchange non-issued assets") {
    for ((buyVersion, sellVersion, exchangeVersion) <- versions) {

      val assetId = exchAsset.id().toString

      val buyer   = acc0
      val seller  = acc1
      val matcher = acc2

      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime

      val buyPrice   = 2 * Order.PriceConstant
      val sellPrice  = 2 * Order.PriceConstant
      val buyAmount  = 1
      val sellAmount = 1

      val pair = AssetPair.createAssetPair("WAVES", assetId).get
      val buy  = Order.buy(buyVersion, buyer, matcher, pair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee)
      val sell = Order.sell(sellVersion, seller, matcher, pair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee)

      val amount = 1
      if (exchangeVersion != 1) {
        val tx = ExchangeTransaction
          .signed(
            TxVersion.V3,
            matcher = matcher,
            order1 = sell,
            order2 = buy,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .right
          .get

        assertBadRequestAndMessage(
          sender.postJson("/transactions/broadcast", tx.json()),
          "Assets should be issued before they can be traded"
        )
      } else {
        val tx = ExchangeTransaction
          .signed(
            1.toByte,
            matcher = matcher,
            order1 = buy,
            order2 = sell,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .right
          .get

        assertBadRequestAndMessage(
          sender.postJson("/transactions/broadcast", tx.json()),
          "Assets should be issued before they can be traded"
        )
      }
    }

  }

  test("negative - check orders v2 and v3 with exchange tx v1") {
    if (sender.findTransactionInfo(exchAsset.id().toString).isEmpty) sender.postJson("/transactions/broadcast", exchAsset.json())
    val pair = AssetPair.createAssetPair("WAVES", exchAsset.id().toString).get

    for ((o1ver, o2ver) <- Seq(
           (2: Byte, 1: Byte),
           (2: Byte, 3: Byte)
         )) {
      val tx        = exchangeTx(pair, matcherFee, orderFee, ntpTime, o1ver, o2ver, acc1, acc0, acc2)
      val sig       = (Json.parse(tx.toString()) \ "proofs").as[Seq[JsString]].head
      val changedTx = tx + ("version" -> JsNumber(1)) + ("signature" -> sig)
      assertBadRequestAndMessage(sender.signedBroadcast(changedTx), "can only contain orders of version 1", 400)
    }
  }

  test("exchange tx with orders v3") {
    val buyer  = acc0
    val seller = acc1

    val assetDescription = "my asset description"

    val IssueTx: IssueTransaction = IssueTransaction(
      TxVersion.V1,
      buyer,
      "myasset".utf8Bytes,
      assetDescription.utf8Bytes,
      quantity = someAssetAmount,
      decimals = 8,
      reissuable = true,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    ).signWith(buyer)

    val assetId = IssueTx.id()

    sender.postJson("/transactions/broadcast", IssueTx.json())

    nodes.waitForHeightAriseAndTxPresent(assetId.toString)

    sender.transfer(firstAddress, secondAddress, IssueTx.quantity / 2, assetId = Some(assetId.toString), waitForTx = true)

    for ((o1ver, o2ver, matcherFeeOrder1, matcherFeeOrder2) <- Seq(
           (1: Byte, 3: Byte, Waves, IssuedAsset(assetId)),
           (1: Byte, 3: Byte, Waves, Waves),
           (2: Byte, 3: Byte, Waves, IssuedAsset(assetId)),
           (3: Byte, 1: Byte, IssuedAsset(assetId), Waves),
           (2: Byte, 3: Byte, Waves, Waves),
           (3: Byte, 2: Byte, IssuedAsset(assetId), Waves)
         )) {

      val matcher                  = pkByAddress(thirdAddress)
      val ts                       = ntpTime.correctedTime()
      val expirationTimestamp      = ts + Order.MaxLiveTime
      var assetBalanceBefore: Long = 0L

      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        assetBalanceBefore = sender.assetBalance(secondAddress, assetId.toString).balance
        sender.transfer(buyer.stringRepr, seller.stringRepr, 100000, minFee, Some(assetId.toString), waitForTx = true)
      }

      val buyPrice   = 500000
      val sellPrice  = 500000
      val buyAmount  = 40000000
      val sellAmount = 40000000
      val assetPair  = AssetPair.createAssetPair("WAVES", assetId.toString).get
      val buy        = Order.buy(o1ver, buyer, matcher, assetPair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee, matcherFeeOrder1)
      val sell       = Order.sell(o2ver, seller, matcher, assetPair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee, matcherFeeOrder2)
      val amount     = 40000000

      val tx =
        ExchangeTransaction
          .signed(
            3.toByte,
            matcher = matcher,
            order1 = buy,
            order2 = sell,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .right
          .get

      sender.postJson("/transactions/broadcast", tx.json())

      nodes.waitForHeightAriseAndTxPresent(tx.id().toString)

      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        sender.assetBalance(secondAddress, assetId.toString).balance shouldBe assetBalanceBefore
      }
    }
  }

  test("exchange tx with orders v4 can use price that is impossible for orders v3/v2/v1") {

    sender.transfer(sender.address, firstAddress, 1000.waves, waitForTx = true)

    val seller        = acc1
    val buyer         = acc0
    val sellerAddress = secondAddress
    val buyerAddress  = firstAddress

    val nftAsset = sender
      .issue(
        seller.toAddress.stringRepr,
        "myNft",
        "myNftDescription",
        quantity = 1L,
        decimals = 0,
        reissuable = false,
        script = None,
        fee = 0.001.waves,
        waitForTx = true
      )
      .id

    val dec6AssetId = sender
      .issue(
        seller.toAddress.stringRepr,
        "some",
        "6 decimals asset",
        quantity = 1000000L,
        decimals = 6,
        reissuable = false,
        script = None,
        fee = 1.waves,
        waitForTx = true
      )
      .id

    val matcher             = pkByAddress(thirdAddress)
    val ts                  = ntpTime.correctedTime()
    val expirationTimestamp = ts + Order.MaxLiveTime
    val amount              = 1
    val nftWavesPrice       = 1000 * math.pow(10, 8).toLong
    val nftForAssetPrice    = 1 * math.pow(10, 8).toLong

    val nftWavesPair      = AssetPair.createAssetPair(nftAsset, "WAVES").get
    val nftOtherAssetPair = AssetPair.createAssetPair(nftAsset, dec6AssetId).get

    val sellNftForWaves = Order.sell(4.toByte, seller, matcher, nftWavesPair, amount, nftWavesPrice, ts, expirationTimestamp, matcherFee, Waves)
    val buyNftForWaves  = Order.buy(4.toByte, buyer, matcher, nftWavesPair, amount, nftWavesPrice, ts, expirationTimestamp, matcherFee, Waves)

    val sellNftForOtherAsset =
      Order.sell(4.toByte, buyer, matcher, nftOtherAssetPair, amount, nftForAssetPrice, ts, expirationTimestamp, matcherFee, Waves)
    val buyNftForOtherAsset =
      Order.buy(4.toByte, seller, matcher, nftOtherAssetPair, amount, nftForAssetPrice, ts, expirationTimestamp, matcherFee, Waves)

    val sellerBalance = sender.balanceDetails(sellerAddress).regular
    val buyerBalance  = sender.balanceDetails(buyerAddress).regular

    val tx =
      ExchangeTransaction
        .signed(
          3.toByte,
          matcher = matcher,
          order1 = buyNftForWaves,
          order2 = sellNftForWaves,
          amount = amount,
          price = nftWavesPrice,
          buyMatcherFee = (BigInt(matcherFee) * amount / sellNftForWaves.amount).toLong,
          sellMatcherFee = (BigInt(matcherFee) * amount / sellNftForWaves.amount).toLong,
          fee = matcherFee,
          timestamp = ntpTime.correctedTime()
        )
        .right
        .get

    sender.signedBroadcast(tx.json(), waitForTx = true)

    sender.nftList(sellerAddress, limit = 1) shouldBe empty
    sender.nftList(buyerAddress, 1).head.assetId shouldBe nftAsset
    sender.balanceDetails(sellerAddress).regular shouldBe sellerBalance + nftWavesPrice - matcherFee
    sender.balanceDetails(buyerAddress).regular shouldBe buyerBalance - nftWavesPrice - matcherFee

    val sellerBalanceAfterFirstExchange = sender.balanceDetails(sellerAddress).regular
    val buyerBalanceAfgerFirstExchange  = sender.balanceDetails(buyerAddress).regular

    val tx2 =
      ExchangeTransaction
        .signed(
          3.toByte,
          matcher = matcher,
          order1 = buyNftForOtherAsset,
          order2 = sellNftForOtherAsset,
          amount = amount,
          price = nftForAssetPrice,
          buyMatcherFee = (BigInt(matcherFee) * amount / buyNftForOtherAsset.amount).toLong,
          sellMatcherFee = (BigInt(matcherFee) * amount / buyNftForOtherAsset.amount).toLong,
          fee = matcherFee,
          timestamp = ntpTime.correctedTime()
        )
        .right
        .get

    sender.signedBroadcast(tx2.json(), waitForTx = true)

    sender.nftList(buyerAddress, limit = 1) shouldBe empty
    sender.nftList(sellerAddress, 1, None).head.assetId shouldBe nftAsset
    sender.assetBalance(sellerAddress, dec6AssetId).balance shouldBe 0
    sender.assetBalance(buyerAddress, dec6AssetId).balance shouldBe 1000000
    sender.balanceDetails(sellerAddress).regular shouldBe sellerBalanceAfterFirstExchange - matcherFee
    sender.balanceDetails(buyerAddress).regular shouldBe buyerBalanceAfgerFirstExchange - matcherFee

  }

  test("failed exchange tx when asset script fails") {
    val seller         = acc0
    val buyer          = acc1
    val matcher        = acc2
    val sellerAddress  = firstAddress
    val buyerAddress   = secondAddress
    val matcherAddress = thirdAddress

    val maxTxsInMicroBlock = sender.config.getInt("waves.miner.max-transactions-in-micro-block")

    val transfers = Seq(
      sender.transfer(sender.address, sellerAddress, 100.waves).id,
      sender.transfer(sender.address, buyerAddress, 100.waves).id,
      sender.transfer(sender.address, matcherAddress, 100.waves).id
    )

    val initScript          = Some(ScriptCompiler.compile("true", ScriptEstimatorV3).right.get._1.bytes().base64)
    val amountAsset         = sender.issue(sellerAddress, "Amount asset", script = initScript, decimals = 8).id
    val priceAsset          = sender.issue(buyerAddress, "Price asset", script = initScript, decimals = 8).id
    val sellMatcherFeeAsset = sender.issue(matcherAddress, "Seller fee asset", script = initScript, decimals = 8).id
    val buyMatcherFeeAsset  = sender.issue(matcherAddress, "Buyer fee asset", script = initScript, decimals = 8).id

    val preconditions = transfers ++ Seq(
      amountAsset,
      priceAsset,
      sellMatcherFeeAsset,
      buyMatcherFeeAsset
    )

    waitForTxs(preconditions)

    val transferToSeller = sender.transfer(matcherAddress, sellerAddress, 1000000000, fee = minFee + smartFee, assetId = Some(sellMatcherFeeAsset)).id
    val transferToBuyer  = sender.transfer(matcherAddress, buyerAddress, 1000000000, fee = minFee + smartFee, assetId = Some(buyMatcherFeeAsset)).id

    waitForTxs(Seq(transferToSeller, transferToBuyer))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + 4 * smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setAssetScriptFee + smartFee + fee * 10

    for ((invalidScriptAsset, owner) <- Seq(
           (amountAsset, sellerAddress),
           (priceAsset, buyerAddress),
           (sellMatcherFeeAsset, matcherAddress),
           (buyMatcherFeeAsset, matcherAddress)
         )) {
      val txs = (1 to maxTxsInMicroBlock * 2).map { _ =>
        mkExchange(buyer, seller, matcher, assetPair, fee, buyMatcherFeeAsset, sellMatcherFeeAsset, buyMatcherFee, sellMatcherFee)
      }
      val txIds        = txs.map(tx => sender.signedBroadcast(tx.json()).id)
      val priorityTxId = updateAssetScript(false, invalidScriptAsset, owner, priorityFee)
      waitForEmptyUtx()
      sender.printDebugMessage(DebugMessage(s"Priority transaction id: $priorityTxId, invalid asset id: $invalidScriptAsset"))

      assertFailedTxs(txIds) // liquid
      nodes.waitForHeightArise()
      assertFailedTxs(txIds) // hardened

      nodes.waitForTransaction(updateAssetScript(true, invalidScriptAsset, owner, setAssetScriptFee + smartFee))
    }
  }

  test("invalid exchange tx when account script fails") {
    val seller         = acc0
    val buyer          = acc1
    val matcher        = acc2
    val sellerAddress  = firstAddress
    val buyerAddress   = secondAddress
    val matcherAddress = thirdAddress

    val maxTxsInMicroBlock = sender.config.getInt("waves.miner.max-transactions-in-micro-block")

    val transfers = Seq(
      sender.transfer(sender.address, sellerAddress, 100.waves).id,
      sender.transfer(sender.address, buyerAddress, 100.waves).id,
      sender.transfer(sender.address, matcherAddress, 100.waves).id
    )

    val amountAsset         = sender.issue(sellerAddress, "Amount asset", decimals = 8).id
    val priceAsset          = sender.issue(buyerAddress, "Price asset", decimals = 8).id
    val sellMatcherFeeAsset = sender.issue(matcherAddress, "Seller fee asset", decimals = 8).id
    val buyMatcherFeeAsset  = sender.issue(matcherAddress, "Buyer fee asset", decimals = 8).id

    val preconditions = transfers ++ Seq(
      amountAsset,
      priceAsset,
      sellMatcherFeeAsset,
      buyMatcherFeeAsset
    )

    waitForTxs(preconditions)

    val transferToSeller = sender.transfer(matcherAddress, sellerAddress, 1000000000, fee = minFee + smartFee, assetId = Some(sellMatcherFeeAsset)).id
    val transferToBuyer  = sender.transfer(matcherAddress, buyerAddress, 1000000000, fee = minFee + smartFee, assetId = Some(buyMatcherFeeAsset)).id

    waitForTxs(Seq(transferToSeller, transferToBuyer))

    val assetPair      = AssetPair.createAssetPair(amountAsset, priceAsset).get
    val fee            = 0.003.waves + smartFee
    val sellMatcherFee = fee / 100000L
    val buyMatcherFee  = fee / 100000L
    val priorityFee    = setScriptFee + smartFee + fee * 10

    for (invalidAccount <- Seq(
           sellerAddress,
           buyerAddress,
           matcherAddress
         )) {
      val txs = (1 to maxTxsInMicroBlock * 2).map { _ =>
        mkExchange(buyer, seller, matcher, assetPair, fee, buyMatcherFeeAsset, sellMatcherFeeAsset, buyMatcherFee, sellMatcherFee)
      }
      val txIds        = txs.map(tx => sender.signedBroadcast(tx.json()).id)
      val priorityTxId = updateAccountScript(Some(false), invalidAccount, priorityFee)
      waitForEmptyUtx()
      sender.printDebugMessage(DebugMessage(s"Priority transaction id: $priorityTxId, invalid account: $invalidAccount"))

      assertInvalidTxs(txIds) // liquid
      nodes.waitForHeightArise()
      assertInvalidTxs(txIds) // hardened

      nodes.waitForTransaction(updateAccountScript(None, invalidAccount, setScriptFee + smartFee))
    }
  }

  private def mkExchange(
      buyer: KeyPair,
      seller: KeyPair,
      matcher: KeyPair,
      assetPair: AssetPair,
      fee: Long,
      buyMatcherFeeAsset: String,
      sellMatcherFeeAsset: String,
      buyMatcherFee: Long,
      sellMatcherFee: Long
  ): ExchangeTransaction = {
    val ts   = ntpTime.getTimestamp()
    val bmfa = Asset.fromString(Some(buyMatcherFeeAsset))
    val smfa = Asset.fromString(Some(sellMatcherFeeAsset))
    val buy  = Order.buy(Order.V4, buyer, matcher, assetPair, 100, 100, ts, ts + Order.MaxLiveTime, buyMatcherFee, bmfa)
    val sell = Order.sell(Order.V4, seller, matcher, assetPair, 100, 100, ts, ts + Order.MaxLiveTime, sellMatcherFee, smfa)
    ExchangeTransaction
      .signed(
        TxVersion.V3,
        matcher,
        buy,
        sell,
        buy.amount,
        buy.price,
        buy.matcherFee,
        sell.matcherFee,
        fee,
        ts
      )
      .right
      .get
  }

  private def updateAssetScript(result: Boolean, asset: String, owner: String, fee: Long): String = {
    sender
      .setAssetScript(
        asset,
        owner,
        fee,
        Some(
          ScriptCompiler
            .compile(
              s"""
               |match tx {
               |  case tx: SetAssetScriptTransaction => true
               |  case _ => $result
               |}
               |""".stripMargin,
              ScriptEstimatorV3
            )
            .right
            .get
            ._1
            .bytes()
            .base64
        )
      )
      .id
  }

  private def updateAccountScript(result: Option[Boolean], account: String, fee: Long): String = {
    sender
      .setScript(
        account,
        result.map { r =>
          ScriptCompiler
            .compile(
              s"""
               {-# STDLIB_VERSION 3 #-}
                 |{-# CONTENT_TYPE EXPRESSION #-}
                 |{-# SCRIPT_TYPE ACCOUNT #-}
                 |
                 |match (tx) {
                 |  case t: ExchangeTransaction => false
                 |  case _ => $r
                 |}
                 |""".stripMargin,
              ScriptEstimatorV3
            )
            .right
            .get
            ._1
            .bytes()
            .base64

        },
        fee = fee
      )
      .id
  }

  def assertFailedTxs(txs: Seq[String]): Unit = {
    val statuses = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.id) < txs.indexOf(s.id) }
    all(statuses.map(_.status)) shouldBe "confirmed"
    all(statuses.map(_.applicationStatus.isDefined)) shouldBe true

    statuses.foreach { s =>
      (sender.transactionInfo[JsObject](s.id) \ "applicationStatus").asOpt[String] shouldBe s.applicationStatus
    }

    val failed = statuses.dropWhile(s => s.applicationStatus.contains("succeed"))
    failed.size should be > 0

    all(failed.flatMap(_.applicationStatus)) shouldBe "scriptExecutionFailed"
  }

  def assertInvalidTxs(txs: Seq[String]): Unit = {
    val statuses = sender.transactionStatus(txs).sortWith { case (f, s) => txs.indexOf(f.id) < txs.indexOf(s.id) }

    statuses.foreach { s =>
      (sender.transactionInfo[JsObject](s.id) \ "applicationStatus").asOpt[String] shouldBe s.applicationStatus
    }

    val invalid = statuses.dropWhile(s => s.applicationStatus.contains("succeed"))
    invalid.size should be > 0

    all(invalid.map(_.status)) shouldBe "not_found"
    all(invalid.map(_.applicationStatus)) shouldBe None
  }

  private def waitForTxs(txs: Seq[String]): Unit =
    nodes.waitFor[Boolean]("preconditions")(100.millis)(
      n => n.transactionStatus(txs).forall(_.status == "confirmed"),
      statuses => statuses.forall(identity)
    )

  private def waitForEmptyUtx(): Unit =
    sender.waitFor("empty utx")(n => n.utxSize, (utxSize: Int) => utxSize == 0, 100.millis)

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5.id.toInt, 0)))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()
}
