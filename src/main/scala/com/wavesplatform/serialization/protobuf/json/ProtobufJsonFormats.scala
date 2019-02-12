package com.wavesplatform.serialization.protobuf.json
import org.json4s.JValue
import org.json4s.JsonAST._
import play.api.libs.json._
import scalapb.GeneratedMessageCompanion

trait ProtobufJsonFormats {
  implicit def generatedMessageJsonFormat[T <: scalapb.GeneratedMessage with scalapb.Message[T]: GeneratedMessageCompanion]: Format[T] = Format[T](
    Reads { js =>
      def toJson4s(js: JsValue): JValue = js match {
        case JsNull          => JNull
        case JsString(value) => JString(value)
        case JsNumber(num)   => JDecimal(num)
        case JsBoolean(bool) => JBool(bool)
        case JsArray(arr)    => JArray(arr.map(toJson4s).toList)
        case obj: JsObject   => JObject(obj.fields.map(kv => (kv._1, toJson4s(kv._2))): _*)
      }

      JsSuccess(PBWavesJsonFormat.fromJson[T](toJson4s(js)))
    },
    Writes { tx =>
      def toPlay(js: JValue): JsValue = js match {
        case JNull | JNothing => JsNull
        case JString(value)   => JsString(value)
        case JInt(num)        => JsNumber(BigDecimal(num))
        case JDecimal(num)    => JsNumber(num)
        case JDouble(num)     => JsNumber(num)
        case JLong(num)       => JsNumber(num)
        case JBool(num)       => JsBoolean(num)
        case JArray(arr)      => JsArray(arr.map(toPlay))
        case JSet(arr)        => JsArray(arr.toVector.map(toPlay))
        case JObject(obj)     => JsObject(obj.map(kv => (kv._1, toPlay(kv._2))))
      }

      toPlay(PBWavesJsonFormat.toJson(tx))
    }
  )
}

object ProtobufJsonFormats extends ProtobufJsonFormats