package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.{NTPTime, NodeConfigs}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.exchangeTx
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils._
import play.api.libs.json.{JsNumber, JsString, Json}

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
            TxVersion.V2,
            matcher = matcher,
            buyOrder = buy,
            sellOrder = sell,
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
            buyOrder = buy,
            sellOrder = sell,
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
            2.toByte,
            matcher = matcher,
            buyOrder = buy,
            sellOrder = sell,
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
          buyOrder = buyNftForWaves,
          sellOrder = sellNftForWaves,
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

    sender.nftAssetsBalance(sellerAddress, limit = 1) shouldBe empty
    sender.nftAssetsBalance(buyerAddress, 1).head.assetId shouldBe nftAsset
    sender.balanceDetails(sellerAddress).regular shouldBe sellerBalance + nftWavesPrice - matcherFee
    sender.balanceDetails(buyerAddress).regular shouldBe buyerBalance - nftWavesPrice - matcherFee


    val sellerBalanceAfterFirstExchange = sender.balanceDetails(sellerAddress).regular
    val buyerBalanceAfgerFirstExchange  = sender.balanceDetails(buyerAddress).regular

    val tx2 =
      ExchangeTransaction
        .signed(
          3.toByte,
          matcher = matcher,
          buyOrder = buyNftForOtherAsset,
          sellOrder = sellNftForOtherAsset,
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

    sender.nftAssetsBalance(buyerAddress, limit = 1) shouldBe empty
    sender.nftAssetsBalance(sellerAddress, 1).head.assetId shouldBe nftAsset
    sender.assetBalance(sellerAddress, dec6AssetId).balance shouldBe 0
    sender.assetBalance(buyerAddress, dec6AssetId).balance shouldBe 1000000
    sender.balanceDetails(sellerAddress).regular shouldBe sellerBalanceAfterFirstExchange - matcherFee
    sender.balanceDetails(buyerAddress).regular shouldBe buyerBalanceAfgerFirstExchange - matcherFee


  }

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5.id.toInt, 0)))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()
}
