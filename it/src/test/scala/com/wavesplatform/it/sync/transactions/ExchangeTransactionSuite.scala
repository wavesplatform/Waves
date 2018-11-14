package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.NTP
import play.api.libs.json._

class ExchangeTransactionSuite extends BaseTransactionSuite {
  test("cannot exchange non-issued assets") {
    for ((o1ver, o2ver, tver) <- Seq(
           (1: Byte, 1: Byte, 1: Byte),
           (1: Byte, 1: Byte, 2: Byte),
           (1: Byte, 2: Byte, 2: Byte),
           (2: Byte, 1: Byte, 2: Byte),
           (2: Byte, 2: Byte, 2: Byte)
         )) {
      val assetName        = "myasset"
      val assetDescription = "my asset description"

      val IssueTx: IssueTransactionV1 = IssueTransactionV1
        .selfSigned(
          sender = sender.privateKey,
          name = assetName.getBytes(),
          description = assetDescription.getBytes(),
          quantity = someAssetAmount,
          decimals = 2,
          reissuable = true,
          fee = 1.waves,
          timestamp = System.currentTimeMillis()
        )
        .right
        .get

      val assetId = IssueTx.id().base58

      val buyer               = pkByAddress(firstAddress)
      val seller              = pkByAddress(secondAddress)
      val matcher             = pkByAddress(thirdAddress)
      val time                = NTP.correctedTime()
      val expirationTimestamp = time + Order.MaxLiveTime
      val buyPrice            = 2 * Order.PriceConstant
      val sellPrice           = 2 * Order.PriceConstant
      val buyAmount           = 1
      val sellAmount          = 1
      val assetPair           = AssetPair.createAssetPair("WAVES", assetId).get
      val buy                 = Order.buy(buyer, matcher, assetPair, buyAmount, buyPrice, time, expirationTimestamp, matcherFee, o1ver)
      val sell                = Order.sell(seller, matcher, assetPair, sellAmount, sellPrice, time, expirationTimestamp, matcherFee, o2ver)

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
            timestamp = NTP.correctedTime()
          )
          .right
          .get

        assertBadRequestAndMessage(
          sender.postJson("/transactions/broadcast", tx.json() + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt))),
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
            timestamp = NTP.correctedTime()
          )
          .right
          .get

        assertBadRequestAndMessage(
          sender.postJson("/transactions/broadcast", tx.json() + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt))),
          "Assets should be issued before they can be traded"
        )
      }
    }

  }

}
