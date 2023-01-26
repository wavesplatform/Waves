package com.wavesplatform.api.http

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import play.api.libs.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue}

object StreamSerializerUtils {
  implicit class JsonWriterOps(val writer: JsonWriter) extends AnyVal {
    def writeKeyValue(key: String, writeVal: JsonWriter => Unit): Unit = {
      writer.writeKey(key)
      writeVal(writer)
    }

    def writeKeyValue(key: String, value: String): Unit = {
      writer.writeKey(key)
      writer.writeVal(value)
    }

    def writeKeyValue(key: String, value: Boolean): Unit = {
      writer.writeKey(key)
      writer.writeVal(value)
    }

    def writeKeyNull(key: String): Unit = {
      writer.writeKey(key)
      writer.writeNull()
    }

    def writeKeyValue(key: String, value: Long, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.lsfFieldNamesToTranslate.contains(key)) {
        writeKeyValue(key, _.writeValAsString(value))
      } else {
        writeKeyValue(key, _.writeVal(value))
      }
    }

    def writeKeyValue(key: String, value: BigDecimal, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.lsfFieldNamesToTranslate.contains(key)) {
        writeKeyValue(key, _.writeValAsString(value))
      } else {
        writeKeyValue(key, _.writeVal(value))
      }
    }

    def writeKeyValue(key: String, value: Int, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.lsfFieldNamesToTranslate.contains(key)) {
        writeKeyValue(key, _.writeValAsString(value))
      } else {
        writeKeyValue(key, _.writeVal(value))
      }
    }

    def writeKeyValue(key: String, value: Byte, numberAsString: Boolean): Unit = {
      if (numberAsString && CustomJson.lsfFieldNamesToTranslate.contains(key)) {
        writeKeyValue(key, _.writeValAsString(value))
      } else {
        writeKeyValue(key, _.writeVal(value))
      }
    }

    def writeKeyValueArray(key: String, writeArrayElems: JsonWriter => Unit): Unit = {
      writer.writeKey(key)
      writer.writeArrayStart()
      writeArrayElems(writer)
      writer.writeArrayEnd()
    }

    def writeKeyValueArray[A](key: String, value: Seq[A])(elemCodec: JsonValueCodec[A]): Unit = {
      writer.writeKey(key)
      writer.writeArrayStart()
      value.foreach(v => elemCodec.encodeValue(v, writer))
      writer.writeArrayEnd()
    }
  }

  def jsValueCodec(numbersAsString: Boolean): JsonValueCodec[JsObject] = new OnlyEncodeJsonValueCodec[JsObject] {
    def encodeValue(jsObj: JsObject, out: JsonWriter): Unit = {
      out.writeObjectStart()
      jsObj.fields.foreach { case (key, value) => encodeField(key, value, out) }
      out.writeObjectEnd()
    }

    private def encodeField(key: String, jsValue: JsValue, out: JsonWriter): Unit = {
      jsValue match {
        case n: JsNumber =>
          out.writeKeyValue(key, n.value, numbersAsString)
        case b: JsBoolean =>
          out.writeKeyValue(key, b.value)
        case s: JsString =>
          out.writeKeyValue(key, s.value)
        case a: JsArray =>
          out.writeKeyValueArray(key, out => a.value.foreach(encodeArrayElem(_, out)))
        case o: JsObject =>
          out.writeKeyValue(key, encodeValue(o, _))
        case _ =>
          out.writeKeyNull(key)
      }
    }

    private def encodeArrayElem(jsValue: JsValue, out: JsonWriter): Unit = {
      jsValue match {
        case n: JsNumber =>
          out.writeVal(n.value)
        case b: JsBoolean =>
          out.writeVal(b.value)
        case s: JsString =>
          out.writeVal(s.value)
        case a: JsArray =>
          out.writeArrayStart()
          a.value.foreach(encodeArrayElem(_, out))
          out.writeArrayEnd()
        case o: JsObject =>
          encodeValue(o, out)
        case _ =>
          out.writeNull()
      }
    }
  }

  trait OnlyEncodeJsonValueCodec[A] extends JsonValueCodec[A] {
    override def decodeValue(in: JsonReader, default: A): A = ???
    override def nullValue: A                               = ???
  }
}
