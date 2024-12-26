package com.wavesplatform.state

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{JsonSerializer, SerializerProvider}
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.api.http.StreamSerializerUtils.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.domain.{DataItem, DataOp}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.DataEntry.*
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.utils.*
import play.api.libs.json.*

sealed abstract class DataEntry[T](val `type`: String, val key: String, val value: T) {
  def valueBytes: Array[Byte]

  def toBytes: Array[Byte] = {
    val keyBytes = key.utf8Bytes
    Bytes.concat(Shorts.toByteArray(keyBytes.length.toShort), keyBytes, valueBytes)
  }

  def toJson: JsObject =
    Json.obj("key" -> key, "type" -> `type`)

  def isValid(version: TxVersion): Boolean = version match {
    case TxVersion.V1 => key.length <= MaxKeySize
    case _            => key.utf8Bytes.length <= MaxPBKeySize
  }
}

object DataEntry {

  type DataBytesOpt = Option[Array[Byte]]

  val MaxKeySize   = 100            // uses for RIDE ContractLimits
  val MaxPBKeySize = 400            //
  val MaxValueSize = Short.MaxValue // uses for RIDE CONST_STRING and CONST_BYTESTR limits

  object Type extends Enumeration {
    val Integer = Value(0)
    val Boolean = Value(1)
    val Binary  = Value(2)
    val String  = Value(3)
  }

  implicit object Format extends Format[DataEntry[?]] {
    def reads(jsv: JsValue): JsResult[DataEntry[?]] = {
      jsv \ "key" match {
        case JsDefined(JsString(key)) =>
          jsv \ "type" match {
            case JsDefined(JsString("integer")) =>
              jsv \ "value" match {
                case JsDefined(JsNumber(n)) => JsSuccess(IntegerDataEntry(key, n.toLong))
                case _                      => JsError("value is missing or not an integer")
              }
            case JsDefined(JsString("boolean")) =>
              jsv \ "value" match {
                case JsDefined(JsBoolean(b)) => JsSuccess(BooleanDataEntry(key, b))
                case _                       => JsError("value is missing or not a boolean value")
              }
            case JsDefined(JsString("binary")) =>
              jsv \ "value" match {
                case JsDefined(JsString(enc)) =>
                  ByteStr.decodeBase64(enc).fold(ex => JsError(ex.getMessage), bstr => JsSuccess(BinaryDataEntry(key, bstr)))
                case _ => JsError("value is missing or not a string")
              }
            case JsDefined(JsString("string")) =>
              jsv \ "value" match {
                case JsDefined(JsString(str)) => JsSuccess(StringDataEntry(key, str))
                case _                        => JsError("value is missing or not a string")
              }
            case JsDefined(JsString(t)) => JsError(s"unknown type $t")
            case _ =>
              jsv \ "value" match {
                case _: JsUndefined | JsDefined(JsNull) => JsSuccess(EmptyDataEntry(key))
                case _                                  => JsError("type is missing")
              }
          }
        case _ => JsError("key is missing")
      }
    }

    def writes(item: DataEntry[?]): JsValue = item.toJson
  }

  def dataEntrySerializer(numberAsString: Boolean): JsonSerializer[DataEntry[?]] =
    (value: DataEntry[?], gen: JsonGenerator, _: SerializerProvider) => {
      gen.writeStartObject()
      value match {
        case BinaryDataEntry(key, value) =>
          gen.writeStringField("key", key)
          gen.writeStringField("type", "binary")
          gen.writeStringField("value", value.base64)
        case IntegerDataEntry(key, value) =>
          gen.writeStringField("key", key)
          gen.writeStringField("type", "integer")
          gen.writeNumberField("value", value, numberAsString)
        case BooleanDataEntry(key, value) =>
          gen.writeStringField("key", key)
          gen.writeStringField("type", "boolean")
          gen.writeBooleanField("value", value)
        case StringDataEntry(key, value) =>
          gen.writeStringField("key", key)
          gen.writeStringField("type", "string")
          gen.writeStringField("value", value)
        case EmptyDataEntry(key) =>
          gen.writeStringField("key", key)
          gen.writeNullField("value")
      }
      gen.writeEndObject()
    }

  implicit class DataEntryExt(private val de: DataEntry[?]) extends AnyVal {
    def isEmpty: Boolean = de.isInstanceOf[EmptyDataEntry]
  }

  def fromLangDataOp(di: DataOp): DataEntry[?] = di match {
    case DataItem.Lng(k, v)  => IntegerDataEntry(k, v)
    case DataItem.Bool(k, v) => BooleanDataEntry(k, v)
    case DataItem.Bin(k, v)  => BinaryDataEntry(k, v)
    case DataItem.Str(k, v)  => StringDataEntry(k, v)
    case DataItem.Delete(k)  => EmptyDataEntry(k)
  }
}

case class IntegerDataEntry(override val key: String, override val value: Long) extends DataEntry[Long]("integer", key, value) {
  override def valueBytes: Array[Byte] = Bytes.concat(Array(Type.Integer.id.toByte), Longs.toByteArray(value))
  override def toJson: JsObject        = super.toJson + ("value" -> JsNumber(value))
}

case class BooleanDataEntry(override val key: String, override val value: Boolean) extends DataEntry[Boolean]("boolean", key, value) {
  override def valueBytes: Array[Byte] = Array(Type.Boolean.id, if (value) 1 else 0).map(_.toByte)
  override def toJson: JsObject        = super.toJson + ("value" -> JsBoolean(value))
}

case class BinaryDataEntry(override val key: String, override val value: ByteStr) extends DataEntry[ByteStr]("binary", key, value) {
  override def valueBytes: Array[Byte]              = Bytes.concat(Array(Type.Binary.id.toByte), Deser.serializeArrayWithLength(value.arr))
  override def toJson: JsObject                     = super.toJson + ("value" -> JsString(value.base64))
  override def isValid(version: TxVersion): Boolean = super.isValid(version) && value.arr.length <= MaxValueSize
}

case class StringDataEntry(override val key: String, override val value: String) extends DataEntry[String]("string", key, value) {
  override def valueBytes: Array[Byte]              = Bytes.concat(Array(Type.String.id.toByte), Deser.serializeArrayWithLength(value.utf8Bytes))
  override def toJson: JsObject                     = super.toJson + ("value" -> JsString(value))
  override def isValid(version: TxVersion): Boolean = super.isValid(version) && value.utf8Bytes.length <= MaxValueSize
}

case class EmptyDataEntry(override val key: String) extends DataEntry[Unit]("empty", key, ()) {
  override def valueBytes: Array[TxVersion] = Array(0xff.toByte)
  override def toJson: JsObject             = Json.obj("key" -> key, "value" -> JsNull)
}
