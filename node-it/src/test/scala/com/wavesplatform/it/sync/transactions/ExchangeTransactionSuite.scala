package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.{CustomValidationError, StateCheckFailed}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.{NodeConfigs, NTPTime}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.sync.smartcontract.exchangeTx
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.assets.exchange.OrderPriceMode.FixedDecimals
import com.wavesplatform.utils.*
import play.api.libs.json.{JsNumber, JsObject, Json, JsString}

class ExchangeTransactionSuite extends BaseTransactionSuite with NTPTime {
  private lazy val exchAsset: IssueTransaction = IssueTransaction(
    TxVersion.V1,
    sender = sender.keyPair.publicKey,
    "myasset".utf8Bytes,
    "my asset description".utf8Bytes,
    quantity = someAssetAmount,
    decimals = 2,
    reissuable = true,
    script = None,
    fee = 1.waves,
    timestamp = System.currentTimeMillis()
  ).signWith(sender.keyPair.privateKey)

  private def acc0 = firstKeyPair
  private def acc1 = secondKeyPair
  private def acc2 = thirdKeyPair

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
      val amount     = 1

      val pair = AssetPair.createAssetPair("WAVES", assetId).get
      val buy  = Order.buy(buyVersion, buyer, matcher.publicKey, pair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee)
      val sell = Order.sell(sellVersion, seller, matcher.publicKey, pair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee)

      val buyFee  = (BigInt(matcherFee) * amount / buy.amount).toLong
      val sellFee = (BigInt(matcherFee) * amount / sell.amount).toLong

      val protoVersion = exchangeVersion > TxVersion.V2

      assertApiError(
        sender.broadcastExchange(matcher, sell, sell, amount, sellPrice, buyFee, sellFee, matcherFee, exchangeVersion, validate = false),
        if (protoVersion) CustomValidationError("buyOrder should has OrderType.BUY") else CustomValidationError("order1 should have OrderType.BUY")
      )

      assertApiError(
        sender.broadcastExchange(matcher, buy, buy, amount, buyPrice, buyFee, sellFee, matcherFee, exchangeVersion, validate = false),
        CustomValidationError("sellOrder should has OrderType.SELL")
      )

      assertApiError {
        if (protoVersion)
          sender.broadcastExchange(matcher, sell, buy, amount, sellPrice, buyFee, sellFee, matcherFee, exchangeVersion)
        else
          sender.broadcastExchange(matcher, buy, sell, amount, sellPrice, buyFee, sellFee, matcherFee, exchangeVersion)
      } { error =>
        error.id shouldBe StateCheckFailed.Id
        error.statusCode shouldBe StateCheckFailed.Code.intValue
        error.message should include("Assets should be issued before they can be traded")
        (error.json \ "transaction").asOpt[JsObject] shouldBe defined
      }
    }
  }

  test("negative - check orders v2 and v3 with exchange tx v1") {
    if (sender.findTransactionInfo(exchAsset.id().toString).isEmpty) sender.postJson("/transactions/broadcast", exchAsset.json())
    val pair = AssetPair.createAssetPair("WAVES", exchAsset.id().toString).get

    for (
      (o1ver, o2ver) <- Seq(
        (2: Byte, 1: Byte),
        (2: Byte, 3: Byte)
      )
    ) {
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
      buyer.publicKey,
      "myasset".utf8Bytes,
      assetDescription.utf8Bytes,
      quantity = someAssetAmount,
      decimals = 8,
      reissuable = true,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    ).signWith(buyer.privateKey)

    val assetId = IssueTx.id()

    sender.postJson("/transactions/broadcast", IssueTx.json())

    nodes.waitForHeightAriseAndTxPresent(assetId.toString)

    sender.transfer(firstKeyPair, secondKeyPair.toAddress.toString, IssueTx.quantity / 2, assetId = Some(assetId.toString), waitForTx = true)

    for (
      (o1ver, o2ver, matcherFeeOrder1, matcherFeeOrder2) <- Seq(
        (1: Byte, 3: Byte, Waves, IssuedAsset(assetId)),
        (1: Byte, 3: Byte, Waves, Waves),
        (2: Byte, 3: Byte, Waves, IssuedAsset(assetId)),
        (3: Byte, 1: Byte, IssuedAsset(assetId), Waves),
        (2: Byte, 3: Byte, Waves, Waves),
        (3: Byte, 2: Byte, IssuedAsset(assetId), Waves)
      )
    ) {

      val matcher                  = thirdKeyPair
      val ts                       = ntpTime.correctedTime()
      val expirationTimestamp      = ts + Order.MaxLiveTime
      var assetBalanceBefore: Long = 0L

      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        assetBalanceBefore = sender.assetBalance(secondKeyPair.toAddress.toString, assetId.toString).balance
        sender.transfer(buyer, seller.toAddress.toString, 100000, minFee, Some(assetId.toString), waitForTx = true)
      }

      val buyPrice   = 500000
      val sellPrice  = 500000
      val buyAmount  = 40000000
      val sellAmount = 40000000
      val assetPair  = AssetPair.createAssetPair("WAVES", assetId.toString).get
      val buy  = Order.buy(o1ver, buyer, matcher.publicKey, assetPair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee, matcherFeeOrder1)
      val sell = Order.sell(o2ver, seller, matcher.publicKey, assetPair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee, matcherFeeOrder2)
      val amount = 40000000

      val tx =
        ExchangeTransaction
          .signed(
            3.toByte,
            matcher = matcher.privateKey,
            order1 = buy,
            order2 = sell,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .explicitGet()

      sender.postJson("/transactions/broadcast", tx.json())

      nodes.waitForHeightAriseAndTxPresent(tx.id().toString)

      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        sender.assetBalance(secondAddress, assetId.toString).balance shouldBe assetBalanceBefore
      }
    }
  }

  test("exchange tx with orders v4 can use price that is impossible for orders v3/v2/v1") {

    sender.transfer(sender.keyPair, firstAddress, 1000.waves, waitForTx = true)

    val seller        = acc1
    val buyer         = acc0
    val sellerKeyPair = secondKeyPair
    val buyerKeyPair  = firstKeyPair

    val nftAsset = sender
      .issue(
        seller,
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
        seller,
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

    val matcher             = thirdKeyPair
    val ts                  = ntpTime.correctedTime()
    val expirationTimestamp = ts + Order.MaxLiveTime
    val amount              = 1
    val nftWavesPrice       = 1000 * math.pow(10, 8).toLong
    val nftForAssetPrice    = 1 * math.pow(10, 8).toLong

    val nftWavesPair      = AssetPair.createAssetPair(nftAsset, "WAVES").get
    val nftOtherAssetPair = AssetPair.createAssetPair(nftAsset, dec6AssetId).get

    val sellNftForWaves =
      Order.sell(4.toByte, seller, matcher.publicKey, nftWavesPair, amount, nftWavesPrice, ts, expirationTimestamp, matcherFee, Waves, FixedDecimals)
    val buyNftForWaves =
      Order.buy(4.toByte, buyer, matcher.publicKey, nftWavesPair, amount, nftWavesPrice, ts, expirationTimestamp, matcherFee, Waves, FixedDecimals)

    val sellNftForOtherAsset =
      Order.sell(
        4.toByte,
        buyer,
        matcher.publicKey,
        nftOtherAssetPair,
        amount,
        nftForAssetPrice,
        ts,
        expirationTimestamp,
        matcherFee,
        Waves,
        FixedDecimals
      )
    val buyNftForOtherAsset =
      Order.buy(
        4.toByte,
        seller,
        matcher.publicKey,
        nftOtherAssetPair,
        amount,
        nftForAssetPrice,
        ts,
        expirationTimestamp,
        matcherFee,
        Waves,
        FixedDecimals
      )

    val sellerAddress = sellerKeyPair.toAddress.toString
    val sellerBalance = sender.balanceDetails(sellerAddress).regular
    val buyerAddress  = buyerKeyPair.toAddress.toString
    val buyerBalance  = sender.balanceDetails(buyerAddress).regular

    val tx =
      ExchangeTransaction
        .signed(
          3.toByte,
          matcher = matcher.privateKey,
          order1 = buyNftForWaves,
          order2 = sellNftForWaves,
          amount = amount,
          price = nftWavesPrice,
          buyMatcherFee = (BigInt(matcherFee) * amount / sellNftForWaves.amount).toLong,
          sellMatcherFee = (BigInt(matcherFee) * amount / sellNftForWaves.amount).toLong,
          fee = matcherFee,
          timestamp = ntpTime.correctedTime()
        )
        .explicitGet()

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
          matcher = matcher.privateKey,
          order1 = buyNftForOtherAsset,
          order2 = sellNftForOtherAsset,
          amount = amount,
          price = nftForAssetPrice,
          buyMatcherFee = (BigInt(matcherFee) * amount / buyNftForOtherAsset.amount).toLong,
          sellMatcherFee = (BigInt(matcherFee) * amount / buyNftForOtherAsset.amount).toLong,
          fee = matcherFee,
          timestamp = ntpTime.correctedTime()
        )
        .explicitGet()

    sender.signedBroadcast(tx2.json(), waitForTx = true)

    sender.nftList(buyerAddress, limit = 1) shouldBe empty
    sender.nftList(sellerAddress, 1, None).head.assetId shouldBe nftAsset
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
