package com.wavesplatform.api.http

import java.io.IOException
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.{MediaRange, MediaType}
import com.fasterxml.jackson.core.io.SegmentedStringWriter
import com.fasterxml.jackson.core.util.BufferRecyclers
import com.fasterxml.jackson.core.{JsonGenerator, JsonProcessingException}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.{JsonMappingException, JsonSerializer, ObjectMapper, SerializerProvider}
import com.wavesplatform.api.http.CustomJson.fieldNamesToTranslate
import play.api.libs.json.*

object NumberAsStringSerializer extends JsonSerializer[JsValue] {
  override def serialize(value: JsValue, json: JsonGenerator, provider: SerializerProvider): Unit =
    serializeWithNumberAsStrings(value, json, provider, insideStringifiedField = false)

  private def serializeWithNumberAsStrings(value: JsValue, json: JsonGenerator, provider: SerializerProvider, insideStringifiedField: Boolean): Unit =
    value match {
      case JsNumber(v) if insideStringifiedField => json.writeString(v.bigDecimal.toPlainString)
      case JsNumber(v) => json.writeNumber(v.bigDecimal)
      case JsString(v) => json.writeString(v)
      case v: JsBoolean => json.writeBoolean(v.value)

      case JsArray(elements) =>
        json.writeStartArray()
        elements.foreach { t =>
          serializeWithNumberAsStrings(t, json, provider, insideStringifiedField)
        }
        json.writeEndArray()

      case JsObject(values) =>
        json.writeStartObject()
        values.foreach {
          case (name, JsNumber(v)) if fieldNamesToTranslate(name) =>
            json.writeStringField(name, v.bigDecimal.toPlainString)
          case (name, jsv) if fieldNamesToTranslate(name) =>
            json.writeFieldName(name)
            serializeWithNumberAsStrings(jsv, json, provider, insideStringifiedField = true)
          case (name, jsv) =>
            json.writeFieldName(name)
            serializeWithNumberAsStrings(jsv, json, provider, insideStringifiedField)
        }
        json.writeEndObject()

      case JsNull => json.writeNull()
    }
}

object CustomJson {
  val jsonWithNumbersAsStrings: MediaType.WithFixedCharset = `application/json`.withParams(Map("large-significand-format" -> "string"))

  val fieldNamesToTranslate = Set(
    "amount",
    "available",
    "balance",
    "buyMatcherFee",
    "currentReward",
    "desiredReward",
    "effective",
    "fee",
    "feeAmount",
    "generating",
    "in",
    "matcherFee",
    "minIncrement",
    "minSponsoredAssetFee",
    "out",
    "price",
    "quantity",
    "regular",
    "reward",
    "rewardShares",
    "sellMatcherFee",
    "sponsorBalance",
    "totalAmount",
    "totalFee",
    "totalWavesAmount",
    "value"
  )

  def acceptsNumbersAsStrings(mr: MediaRange): Boolean = mr match {
    case MediaRange.One(`jsonWithNumbersAsStrings`, _) => true
    case _                                             => false
  }

  private lazy val mapper = (new ObjectMapper)
    .registerModule(new SimpleModule("WavesJson").addSerializer(classOf[JsValue], NumberAsStringSerializer))
    .configure(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN, true)

  def writeValueAsString(value: JsValue): String = {
    val sw = new SegmentedStringWriter(BufferRecyclers.getBufferRecycler)
    try mapper.writeValue(sw, value)
    catch {
      case e: JsonProcessingException =>
        throw e
      case e: IOException =>
        // shouldn't really happen, but is declared as possibility so:
        throw JsonMappingException.fromUnexpectedIOE(e)
    }
    sw.getAndClear
  }
}
