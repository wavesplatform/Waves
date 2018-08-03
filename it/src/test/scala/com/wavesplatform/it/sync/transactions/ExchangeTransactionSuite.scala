package com.wavesplatform.it.sync.transactions

import com.wavesplatform.api.http.assets.{SignedExchangeRequest, SignedExchangeRequestV2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.util._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.utils.{Base58, NTP}
import play.api.libs.json._

class ExchangeTransactionSuite extends BaseTransactionSuite {

  implicit val w1 = Json.writes[SignedExchangeRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt)))
  implicit val w2 =
    Json.writes[SignedExchangeRequestV2].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt)))

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
      val seller              = pkByAddress(firstAddress)
      val matcher             = pkByAddress(thirdAddress)
      val time                = NTP.correctedTime()
      val expirationTimestamp = time + Order.MaxLiveTime
      val buyPrice            = 2 * Order.PriceConstant
      val sellPrice           = 2 * Order.PriceConstant
      val buyAmount           = 1
      val sellAmount          = 1
      val assetPair           = AssetPair.createAssetPair("WAVES", assetId).get
      val buy                 = Order.buy(buyer, matcher, assetPair, buyPrice, buyAmount, time, expirationTimestamp, matcherFee, o1ver)
      val sell                = Order.sell(seller, matcher, assetPair, sellPrice, sellAmount, time, expirationTimestamp, matcherFee, o2ver)

      val amount = 1
      if (tver != 1) {
        val tx = ExchangeTransactionV2
          .create(
            matcher = matcher,
            buyOrder = buy,
            sellOrder = sell,
            price = sellPrice,
            amount = amount,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = NTP.correctedTime()
          )
          .right
          .get

        def request(tx: ExchangeTransaction): SignedExchangeRequestV2 =
          SignedExchangeRequestV2(
            Base58.encode(tx.sender.publicKey),
            tx.buyOrder,
            tx.sellOrder,
            tx.price,
            tx.amount,
            matcherFee,
            tx.buyMatcherFee,
            tx.sellMatcherFee,
            tx.timestamp,
            2: Byte,
            tx.proofs.proofs.map(_.base58).toList //signature.base58
          )

        assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", request(tx)), "Assets should be issued before they can be traded")
      } else {
        val tx = ExchangeTransactionV1
          .create(
            matcher = matcher,
            buyOrder = buy.asInstanceOf[OrderV1],
            sellOrder = sell.asInstanceOf[OrderV1],
            price = sellPrice,
            amount = amount,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = NTP.correctedTime()
          )
          .right
          .get

        def request(tx: ExchangeTransaction): SignedExchangeRequest =
          SignedExchangeRequest(
            Base58.encode(tx.sender.publicKey),
            tx.buyOrder,
            tx.sellOrder,
            tx.price,
            tx.amount,
            matcherFee,
            tx.buyMatcherFee,
            tx.sellMatcherFee,
            tx.timestamp,
            tx.proofs.proofs(0).base58
          )

        assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", request(tx)), "Assets should be issued before they can be traded")
      }
    }

  }

}
