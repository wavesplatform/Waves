package com.wavesplatform.state2

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.DataEntry._
import play.api.libs.json._
import scorex.crypto.encode.Base58
import scorex.serialization.Deser

import scala.util.Success


sealed abstract class DataEntry[T](val key: String, val value: T) {
  def valueBytes: Array[Byte]

  def toBytes: Array[Byte] = {
    val keyBytes = key.getBytes(UTF8)
    Bytes.concat(Array(keyBytes.length.toByte), keyBytes, valueBytes)
  }

  def toJson: JsObject = Json.obj("key" -> key)
  def valid: Boolean = key.getBytes(UTF8).length <= MaxKeySize
}


object DataEntry {
  val MaxKeySize = Byte.MaxValue
  val MaxValueSize = 1024

  private val UTF8 = StandardCharsets.UTF_8

  object Type extends Enumeration {
    val Integer = Value(0)
    val Boolean = Value(1)
    val Binary = Value(2)
  }

  def parse(bytes: Array[Byte], p: Int): (DataEntry[_], Int) = {
    val keyLength = bytes(p)
    val key = new String(bytes, p + 1, keyLength, UTF8)
    parseValue(key, bytes, p + 1 + keyLength)
  }

  def parseValue(key: String, bytes: Array[Byte], p: Int): (DataEntry[_], Int) = {
    bytes(p) match {
      case t if t == Type.Integer.id => (LongDataEntry(key, Longs.fromByteArray(bytes.drop(p + 1))), p + 9)
      case t if t == Type.Boolean.id => (BooleanDataEntry(key, bytes(p + 1) != 0), p + 2)
      case t if t == Type.Binary.id =>
        val (blob, p1) = Deser.parseArraySize(bytes, p + 1)
        (BinaryDataEntry(key, blob), p1)
      case t => throw new Exception(s"Unknown type $t")
    }
  }

  implicit object Format extends Format[DataEntry[_]] {
    def reads(jsv: JsValue): JsResult[DataEntry[_]] = {
      jsv \ "key" match {
        case JsDefined(JsString(key)) =>
          jsv \ "type" match {
            case JsDefined(JsString("integer")) => jsv \ "value" match {
              case JsDefined(JsNumber(n)) => JsSuccess(LongDataEntry(key, n.toLong))
              case _ => JsError("value is missing or not an integer")
            }
            case JsDefined(JsString("boolean")) => jsv \ "value" match {
              case JsDefined(JsBoolean(b)) => JsSuccess(BooleanDataEntry(key, b))
              case _ => JsError("value is missing or not a boolean value")
            }
            case JsDefined(JsString("binary")) => jsv \ "value" match {
              case JsDefined(JsString(base58)) =>
                val t = if (base58.isEmpty) Success(Array.emptyByteArray) else Base58.decode(base58) /// Base58 bug
                t.fold(
                  ex => JsError(ex.getMessage),
                  arr => JsSuccess(BinaryDataEntry(key, arr)))
              case _ => JsError("value is missing or not a string")
            }
            case JsDefined(JsString(t)) => JsError(s"unknown type $t")
            case _ => JsError("type is missing")
          }
        case _ => JsError("key is missing")
      }
    }

    def writes(item: DataEntry[_]): JsValue = item.toJson
  }
}

case class LongDataEntry(override val key: String, override val value: Long) extends DataEntry[Long](key, value) {
  override def valueBytes: Array[Byte] = Type.Integer.id.toByte +: Longs.toByteArray(value)

  override def toJson: JsObject = super.toJson + ("type" -> JsString("integer")) + ("value" -> JsNumber(value))
}

case class BooleanDataEntry(override val key: String, override val value: Boolean) extends DataEntry[Boolean](key, value) {
  override def valueBytes: Array[Byte] = Array(Type.Boolean.id, if (value) 1 else 0).map(_.toByte)

  override def toJson: JsObject = super.toJson + ("type" -> JsString("boolean")) + ("value" -> JsBoolean(value))
}

case class BinaryDataEntry(override val key: String, override val value: Array[Byte]) extends DataEntry[Array[Byte]](key, value) {
  override def valueBytes: Array[Byte] = Type.Binary.id.toByte +: Deser.serializeArray(value)

  override def toJson: JsObject = super.toJson + ("type" -> JsString("binary")) + ("value" -> JsString(Base58.encode(value)))

  override def valid: Boolean = super.valid && value.length <= MaxValueSize
}
