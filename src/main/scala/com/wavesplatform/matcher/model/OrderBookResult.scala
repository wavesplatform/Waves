package com.wavesplatform.matcher.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.SerializerProvider
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.wavesplatform.matcher.api.JsonSerializer
import play.api.libs.json.{Json, Writes}
import scorex.transaction.assets.exchange.AssetPair
import scorex.utils.NTP

@JsonSerialize(using = classOf[OrderBookResult.Serializer])
case class OrderBookResult(timestamp: Long, pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg])

object OrderBookResult {

  def empty(pair: AssetPair) = OrderBookResult(NTP.correctedTime(), pair, Seq.empty, Seq.empty)

  implicit val assetPairWrites = new Writes[AssetPair] {
    def writes(pair: AssetPair) = Json.obj(
      "amountAsset" -> pair.amountAssetStr,
      "priceAsset"  -> pair.priceAssetStr
    )
  }

  implicit val levelAggtWrites = new Writes[LevelAgg] {
    def writes(a: LevelAgg) = Json.obj(
      "price"  -> a.price,
      "amount" -> a.amount
    )
  }

  implicit val OrderBookResultWrites = new Writes[OrderBookResult] {
    def writes(ob: OrderBookResult) = Json.obj(
      "timestamp" -> ob.timestamp,
      "pair"      -> ob.pair,
      "bids"      -> ob.bids,
      "asks"      -> ob.asks
    )
  }

  def toJson(x: OrderBookResult): String = JsonSerializer.serialize(x)

  class Serializer extends StdSerializer[OrderBookResult](classOf[OrderBookResult]) {
    override def serialize(x: OrderBookResult, j: JsonGenerator, serializerProvider: SerializerProvider): Unit = {
      j.writeStartObject()

      j.writeNumberField("timestamp", x.timestamp)

      j.writeFieldName("pair")
      j.writeStartObject()
      j.writeStringField("amountAsset", x.pair.amountAssetStr)
      j.writeStringField("priceAsset", x.pair.priceAssetStr)
      j.writeEndObject()

      j.writeArrayFieldStart("bids")
      x.bids.foreach(j.writeObject)
      j.writeEndArray()

      j.writeArrayFieldStart("asks")
      x.asks.foreach(j.writeObject)
      j.writeEndArray()

      j.writeEndObject()
    }
  }
}
