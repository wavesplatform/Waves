package com.wavesplatform.transaction.transfer

import java.nio.charset.StandardCharsets

import com.google.common.primitives.Longs
import com.wavesplatform.common.utils.{Base58, Base64}
import play.api.libs.json._

sealed trait Attachment

object Attachment {
  final case class Num(value: Long) extends Attachment

  final case class Bool(value: Boolean) extends Attachment

  final case class Bin(value: Array[Byte]) extends Attachment

  final case class Str(value: String) extends Attachment

  implicit class AttachmentExt(private val a: Option[Attachment]) extends AnyVal {
    def toBytes: Array[Byte] = a.fold(Array.emptyByteArray) {
      case Bin(value)  => value
      case Num(value)  => Longs.toByteArray(value)
      case Bool(value) => if (value) Array(1: Byte) else Array(0: Byte)
      case Str(value)  => value.getBytes(StandardCharsets.UTF_8)
    }

    def toBytesStrict: Array[Byte] = a.fold(Array.emptyByteArray) {
      case Bin(value) => value
      case other => throw new IllegalArgumentException(s"$other can not be strictly converted to bytes")
    }

    def toJson(useTypedFormat: Boolean): JsValue =
      if (useTypedFormat) Json.toJson(a)
      else
        a.fold(JsString("")) {
          case Bin(value) => JsString(Base58.encode(value))
          case other      => throw new IllegalArgumentException(s"$other is typed")
        }
  }

  implicit val jsonFormat: Format[Attachment] = {
    implicit val bytesFormat: Format[Array[Byte]] = Format[Array[Byte]](
      Reads {
        case JsString(value) => JsSuccess(Base64.decode(value))
        case _               => JsError("Expected Base64 string")
      },
      Writes(bs => JsString(Base64.encode(bs)))
    )

    Format(
      Reads {
        case o: JsObject =>
          val result = (o \ "type").as[String] match {
            case "integer" => Num((o \ "value").as[Long])
            case "boolean" => Bool((o \ "value").as[Boolean])
            case "binary"  => Bin((o \ "value").as[Array[Byte]])
            case "string"  => Str((o \ "value").as[String])
            case v         => throw new IllegalArgumentException(s"Attachment type not supported: $v")
          }
          JsSuccess(result)

        case JsString(v) =>
          if (v.length <= TransferTransaction.MaxAttachmentStringSize) JsSuccess(Bin(Base58.decode(v)))
          else JsError(s"attachment length ${v.length} exceeds maximum length of ${TransferTransaction.MaxAttachmentStringSize}")
        case _ =>
          JsError("Expected object or null")
      },
      Writes {
        case Num(value)  => Json.obj("type" -> "integer", "value" -> value)
        case Bool(value) => Json.obj("type" -> "boolean", "value" -> value)
        case Bin(value)  => Json.obj("type" -> "binary", "value"  -> value)
        case Str(value)  => Json.obj("type" -> "string", "value"  -> value)
      }
    )
  }
}
