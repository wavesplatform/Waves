package com.wavesplatform.api.http

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}

object StreamSerializerUtils {
  implicit class JsonGeneratorOps(val gen: JsonGenerator) extends AnyVal {
    def writeValueField[A](key: String, value: A)(ser: JsonSerializer[A], provider: SerializerProvider): Unit = {
      gen.writeFieldName(key)
      ser.serialize(value, gen, provider)
    }

    def writeValueField[A](key: String)(writeF: JsonGenerator => Unit): Unit = {
      gen.writeFieldName(key)
      writeF(gen)
    }

    def writeNumberField(key: String, value: Long, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.fieldNamesToTranslate.contains(key)) {
        gen.writeStringField(key, value.toString)
      } else {
        gen.writeNumberField(key, value)
      }
    }

    def writeNumberField(key: String, value: BigDecimal, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.fieldNamesToTranslate.contains(key)) {
        gen.writeStringField(key, value.bigDecimal.toPlainString)
      } else {
        gen.writeNumberField(key, value.bigDecimal)
      }
    }

    def writeNumberField(key: String, value: Int, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.fieldNamesToTranslate.contains(key)) {
        gen.writeStringField(key, value.toString)
      } else {
        gen.writeNumberField(key, value)
      }
    }

    def writeNumberField(key: String, value: Byte, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.fieldNamesToTranslate.contains(key)) {
        gen.writeStringField(key, value.toString)
      } else {
        gen.writeNumberField(key, value)
      }
    }

    def writeArrayField(key: String)(writeArrayElems: JsonGenerator => Unit): Unit = {
      gen.writeFieldName(key)
      gen.writeStartArray()
      writeArrayElems(gen)
      gen.writeEndArray()
    }

    def writeArrayField[A](key: String, value: Seq[A])(ser: JsonSerializer[A], provider: SerializerProvider): Unit = {
      gen.writeFieldName(key)
      gen.writeStartArray()
      value.foreach(v => ser.serialize(v, gen, provider))
      gen.writeEndArray()
    }
  }
}
