package com.wavesplatform.matcher.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.SerializerProvider
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.wavesplatform.matcher.api.JsonSerializer
import com.wavesplatform.transaction.assets.exchange.AssetPair

@JsonSerialize(using = classOf[OrderBookResult.Serializer])
case class OrderBookResult(timestamp: Long, pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg], assetPairDecimals: Option[(Int, Int)] = None)

object OrderBookResult {

  private def formatValue(value: Double, decimals: Int): String = new java.text.DecimalFormat(s"0.${"0" * decimals}").format(value)

  private def denormalizeAndSerializeSide(side: Seq[LevelAgg], amountAssetDecimals: Int, priceAssetDecimals: Int, jg: JsonGenerator): Unit = {
    side.foreach { levelAgg =>
      val denormalizedPrice  = MatcherModel.Denormalization.denormalizePrice(levelAgg.price, amountAssetDecimals, priceAssetDecimals)
      val denormalizedAmount = MatcherModel.Denormalization.denormalizeAmountAndFee(levelAgg.amount, amountAssetDecimals)

      jg.writeStartArray(2)
      jg.writeString(formatValue(denormalizedPrice, priceAssetDecimals))
      jg.writeString(formatValue(denormalizedAmount, amountAssetDecimals))
      jg.writeEndArray()
    }
  }

  def toJson(x: OrderBookResult): String = JsonSerializer.serialize(x)

  class Serializer extends StdSerializer[OrderBookResult](classOf[OrderBookResult]) {
    override def serialize(x: OrderBookResult, j: JsonGenerator, serializerProvider: SerializerProvider): Unit = {

      j.writeStartObject()
      j.writeNumberField("timestamp", x.timestamp)

      x.assetPairDecimals.fold {

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

      } {
        case (amountAssetDecimals, priceAssetDecimals) =>
          j.writeArrayFieldStart("bids")
          denormalizeAndSerializeSide(x.bids, amountAssetDecimals, priceAssetDecimals, j)
          j.writeEndArray()

          j.writeArrayFieldStart("asks")
          denormalizeAndSerializeSide(x.asks, amountAssetDecimals, priceAssetDecimals, j)
          j.writeEndArray()
      }

      j.writeEndObject()
    }
  }
}
