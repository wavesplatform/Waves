package com.wavesplatform.transaction.transfer

import java.nio.charset.StandardCharsets

import com.google.common.primitives.Longs
import com.wavesplatform.common.utils.Base64
import play.api.libs.json._

sealed trait Attachment {
  def size: Int
}

object Attachment {
  final case class Num(value: Long) extends Attachment {
    override def size: Int = 8
  }

  final case class Bool(value: Boolean) extends Attachment {
    override def size: Int = 1
  }

  final case class Bin(value: Array[Byte]) extends Attachment {
    override def size: Int = value.length
  }

  final case class Str(value: String) extends Attachment {
    override lazy val size: Int = value.getBytes(StandardCharsets.UTF_8).length
  }

  final case object Empty extends Attachment {
    override def size: Int = 0
  }

  def fromBytes(bs: Array[Byte]): Attachment =
    if (bs.isEmpty) Empty else Bin(bs)

  implicit class AttachmentExt(private val a: Attachment) extends AnyVal {
    def asBytes: Array[Byte] = a match {
      case Bin(value)  => value
      case Num(value)  => Longs.toByteArray(value)
      case Bool(value) => if (value) Array(1: Byte) else Array(0: Byte)
      case Str(value)  => value.getBytes(StandardCharsets.UTF_8)
      case Empty       => Array.emptyByteArray
    }

    def asBytesExactly: Array[Byte] = a match {
      case Bin(value) => value
      case Empty      => Array.emptyByteArray
      case _          => throw new IllegalArgumentException(s"Not a bytes attachment: $a")
    }
  }

  implicit val jsonFormat: Format[Attachment] = {
    implicit val bytesFormat = Format[Array[Byte]](
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

        case JsNull =>
          JsSuccess(Empty)

        case _ =>
          JsError("Expected object or null")
      },
      Writes {
        case Num(value)  => Json.obj("type" -> "integer", "value" -> value)
        case Bool(value) => Json.obj("type" -> "boolean", "value" -> value)
        case Bin(value)  => Json.obj("type" -> "binary", "value" -> value)
        case Str(value)  => Json.obj("type" -> "string", "value" -> value)
        case Empty       => JsNull
      }
    )
  }
}
