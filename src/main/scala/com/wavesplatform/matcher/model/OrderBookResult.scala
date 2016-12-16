package com.wavesplatform.matcher.model

import play.api.libs.json.{Json, Writes}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.AssetPair

case class OrderBookResult(timestamp: Long, pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg]) {

}

object OrderBookResult {
  implicit val assetPairWrites = new Writes[AssetPair] {
    def writes(pair: AssetPair) = Json.obj(
      "asset1" -> pair.first.map(Base58.encode),
      "asset2" -> pair.second.map(Base58.encode)
    )
  }

  implicit val levelAggtWrites = new Writes[LevelAgg] {
    def writes(a: LevelAgg) = Json.obj(
      "price" -> a.price,
      "amount" -> a.amount
    )
  }

  implicit val OrderBookResultWrites = new Writes[OrderBookResult] {
    def writes(ob: OrderBookResult) = Json.obj(
      "timestamp" -> ob.timestamp,
      "pair" -> ob.pair,
      "bids" -> ob.bids,
      "asks" -> ob.asks
    )
  }
}