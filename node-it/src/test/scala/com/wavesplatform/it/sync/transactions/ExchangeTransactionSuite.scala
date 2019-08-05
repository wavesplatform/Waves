package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.smartcontract.exchangeTx
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange._
import play.api.libs.json.{JsNumber, JsString, Json}

class ExchangeTransactionSuite extends BaseTransactionSuite with NTPTime {
  var exchAsset: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = sender.privateKey,
      name = "myasset".getBytes("UTF-8"),
      description = "my asset description".getBytes("UTF-8"),
      quantity = someAssetAmount,
      decimals = 2,
      reissuable = true,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  var pair: AssetPair = _

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  val transactionV1versions = (1: Byte, 1: Byte, 1: Byte) // in ExchangeTransactionV1 only orders V1 are supported
  val transactionV2versions = for {
    o1ver <- 1 to 3
    o2ver <- 1 to 3
  } yield (o1ver.toByte, o2ver.toByte, 2.toByte)

  val versions = transactionV1versions +: transactionV2versions

  test("cannot exchange non-issued assets") {
    for ((o1ver, o2ver, tver) <- versions) {

      val assetId = exchAsset.id().base58

      val buyer               = acc0
      val seller              = acc1
      val matcher             = acc2
      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime
      val buyPrice            = 2 * Order.PriceConstant
      val sellPrice           = 2 * Order.PriceConstant
      val buyAmount           = 1
      val sellAmount          = 1
      pair = AssetPair.createAssetPair("WAVES", assetId).get
      val buy  = Order.buy(buyer, matcher, pair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee, o1ver)
      val sell = Order.sell(seller, matcher, pair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee, o2ver)

      val amount = 1
      if (tver != 1) {
        val tx = ExchangeTransactionV2
          .create(
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
        val tx = ExchangeTransactionV1
          .create(
            matcher = matcher,
            buyOrder = buy.asInstanceOf[OrderV1],
            sellOrder = sell.asInstanceOf[OrderV1],
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
    if (sender.findTransactionInfo(exchAsset.id().base58).isEmpty) sender.postJson("/transactions/broadcast", exchAsset.json())
    pair = AssetPair.createAssetPair("WAVES", exchAsset.id().base58).get

    for ((o1ver, o2ver) <- Seq(
           (2: Byte, 1: Byte),
           (2: Byte, 3: Byte),
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

    val IssueTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = buyer,
        name = "myasset".getBytes("UTF-8"),
        description = assetDescription.getBytes("UTF-8"),
        quantity = someAssetAmount,
        decimals = 8,
        reissuable = true,
        fee = 1.waves,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    val assetId = IssueTx.id()

    sender.postJson("/transactions/broadcast", IssueTx.json())

    nodes.waitForHeightAriseAndTxPresent(assetId.base58)

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
      var assetBalanceBefore: Long = 0l

      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        assetBalanceBefore = sender.assetBalance(secondAddress, assetId.base58).balance
        sender.transfer(buyer.stringRepr, seller.stringRepr, 100000, minFee, Some(assetId.base58), waitForTx = true)
      }

      val buyPrice   = 500000
      val sellPrice  = 500000
      val buyAmount  = 40000000
      val sellAmount = 40000000
      val assetPair  = AssetPair.createAssetPair("WAVES", assetId.base58).get
      val buy        = Order.buy(buyer, matcher, assetPair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee, o1ver, matcherFeeOrder1)
      val sell       = Order.sell(seller, matcher, assetPair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee, o2ver, matcherFeeOrder2)
      val amount     = 40000000

      val tx =
        ExchangeTransactionV2
          .create(
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

      nodes.waitForHeightAriseAndTxPresent(tx.id().base58)

      if (matcherFeeOrder1 == Waves && matcherFeeOrder2 != Waves) {
        sender.assetBalance(secondAddress, assetId.base58).balance shouldBe assetBalanceBefore
      }
    }
  }
}
