package com.wavesplatform.state

import java.nio.charset.StandardCharsets.UTF_8

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.Deser
import com.wavesplatform.state.DataEntry._
import play.api.libs.json._

sealed abstract class DataEntry[T](val `type`: String, val key: String, val value: T)(implicit val dataBytesOpt: DataBytesOpt) {
  def valueBytes: Array[Byte]

  def toBytes: Array[Byte] = {
    dataBytesOpt.getOrElse {
      val keyBytes = key.getBytes(UTF_8)
      Bytes.concat(Shorts.toByteArray(keyBytes.length.toShort), keyBytes, valueBytes)
    }
  }

  def toJson: JsObject = Json.obj("key" -> key, "type" -> `type`)
  def valid: Boolean   = key.length <= MaxKeySize
}

object DataEntry {

  type DataBytesOpt = Option[Array[Byte]]

  val MaxKeySize: Byte = 100
  val MaxValueSize     = Short.MaxValue

  object Type extends Enumeration {
    val Integer = Value(0)
    val Boolean = Value(1)
    val Binary  = Value(2)
    val String  = Value(3)
  }

  def parse(bytes: Array[Byte], p: Int): (DataEntry[_], Int) = {
    val keyLength = Shorts.fromByteArray(bytes.drop(p))
    val key       = new String(bytes, p + 2, keyLength, UTF_8)
    parseValue(key, bytes, p + 2 + keyLength)
  }

  def parseValue(key: String, bytes: Array[Byte], p: Int): (DataEntry[_], Int) = {
    bytes(p) match {
      case t if t == Type.Integer.id => (IntegerDataEntry(key, Longs.fromByteArray(bytes.drop(p + 1))), p + 9)
      case t if t == Type.Boolean.id => (BooleanDataEntry(key, bytes(p + 1) != 0), p + 2)
      case t if t == Type.Binary.id =>
        val (blob, p1) = Deser.parseArrayWithLength(bytes, p + 1)
        (BinaryDataEntry(key, ByteStr(blob)), p1)

      case t if t == Type.String.id =>
        val (blob, p1)                          = Deser.parseArrayWithLength(bytes, p + 1)
        implicit val dataBytesOpt: DataBytesOpt = Some(bytes.slice(p - 2 - key.getBytes("UTF-8").length, p1)) //all bytes of this data entry
        (StringDataEntry(key, new String(blob, UTF_8)), p1)
      case t => throw new Exception(s"Unknown type $t")
    }
  }

  implicit object Format extends Format[DataEntry[_]] {
    def reads(jsv: JsValue): JsResult[DataEntry[_]] = {
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
            case _                      => JsError("type is missing")
          }
        case _ => JsError("key is missing")
      }
    }

    def writes(item: DataEntry[_]): JsValue = item.toJson
  }
}

case class IntegerDataEntry(override val key: String, override val value: Long)(implicit dataBytesOpt: DataBytesOpt = None)
    extends DataEntry[Long]("integer", key, value) {
  override def valueBytes: Array[Byte] = Type.Integer.id.toByte +: Longs.toByteArray(value)

  override def toJson: JsObject = super.toJson + ("value" -> JsNumber(value))
}

case class BooleanDataEntry(override val key: String, override val value: Boolean)(implicit dataBytesOpt: DataBytesOpt = None)
    extends DataEntry[Boolean]("boolean", key, value) {
  override def valueBytes: Array[Byte] = Array(Type.Boolean.id, if (value) 1 else 0).map(_.toByte)

  override def toJson: JsObject = super.toJson + ("value" -> JsBoolean(value))
}

case class BinaryDataEntry(override val key: String, override val value: ByteStr)(implicit dataBytesOpt: DataBytesOpt = None)
    extends DataEntry[ByteStr]("binary", key, value) {
  override def valueBytes: Array[Byte] = Type.Binary.id.toByte +: Deser.serializeArray(value.arr)

  override def toJson: JsObject = super.toJson + ("value" -> JsString(value.base64))

  override def valid: Boolean = super.valid && value.arr.length <= MaxValueSize
}

case class StringDataEntry(override val key: String, override val value: String)(implicit dataBytesOpt: DataBytesOpt = None)
    extends DataEntry[String]("string", key, value) {
  override def valueBytes: Array[Byte] = Type.String.id.toByte +: Deser.serializeArray(value.getBytes(UTF_8))

  override def toJson: JsObject = super.toJson + ("value" -> JsString(value))

  override def valid: Boolean = super.valid && value.getBytes(UTF_8).length <= MaxValueSize
}
