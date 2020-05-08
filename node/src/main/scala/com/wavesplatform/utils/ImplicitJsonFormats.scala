package com.wavesplatform.utils

import cats.data.Ior
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.ByteStr.{decodeBase58, decodeBase64}
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.state.{Height, TransactionId}
import play.api.libs.json._

object ImplicitJsonFormats {
  implicit val byteStrFormat: Format[ByteStr] = com.wavesplatform.utils.byteStrFormat

  implicit val byteStringFormat: Format[ByteString] = new Format[ByteString] {
    override def writes(o: ByteString): JsValue = JsString(ByteStr(o.toByteArray).toString)

    override def reads(json: JsValue): JsResult[ByteString] = json match {
      case JsString(v) if v.startsWith("base64:") =>
        decodeBase64(v.substring(7)).fold(e => JsError(s"Error parsing base64: ${e.getMessage}"), b => JsSuccess(ByteString.copyFrom(b.arr)))
      case JsString(v) => decodeBase58(v).fold(e => JsError(s"Error parsing base58: ${e.getMessage}"), b => JsSuccess(ByteString.copyFrom(b.arr)))
      case _           => JsError("Expected JsString")
    }
  }

  implicit def iorFormat[A: Format, B: Format]: Format[Ior[A, B]] = new Format[Ior[A, B]] {
    override def reads(json: JsValue): JsResult[Ior[A, B]] = {
      val left  = (json \ "left").asOpt[A]
      val right = (json \ "right").asOpt[B]
      Ior.fromOptions(left, right) match {
        case Some(value) => JsSuccess(value)
        case None        => JsError("Expected left or right")
      }
    }

    override def writes(o: Ior[A, B]): JsValue = o match {
      case Ior.Left(a)    => Json.obj("left"  -> a)
      case Ior.Right(b)   => Json.obj("right" -> b)
      case Ior.Both(a, b) => Json.obj("left"  -> a, "right" -> b)
    }
  }

  implicit val scriptFormat: Format[Script] = Format[Script](
    implicitly[Reads[ByteStr]].map(bs => ScriptReader.fromBytes(bs.arr).explicitGet()),
    Writes(sc => JsString(sc.bytes().base64))
  )

  implicit val aliasFormat: Format[Alias] = Format[Alias](
    implicitly[Reads[String]].map(bs => Alias.create(bs).explicitGet()),
    Writes(alias => JsString(alias.name))
  )

  implicit val heightFormat: Format[Height]               = implicitly[Format[Int]].asInstanceOf[Format[Height]]
  implicit val publicKeyFormat: Format[PublicKey]         = implicitly[Format[ByteStr]].asInstanceOf[Format[PublicKey]]
  implicit val transactionIdFormat: Format[TransactionId] = implicitly[Format[ByteStr]].asInstanceOf[Format[TransactionId]]

  implicit def anyOptFormat[V: Format]: Format[Option[V]] = new Format[Option[V]] {
    override def reads(json: JsValue): JsResult[Option[V]] = json match {
      case JsNull => JsSuccess(None)
      case _      => implicitly[Format[V]].reads(json).map(Some(_))
    }

    override def writes(o: Option[V]): JsValue = o match {
      case None        => JsNull
      case Some(value) => implicitly[Format[V]].writes(value)
    }
  }

  implicit def anyMapFormat[K: Format, V: Format]: Format[Map[K, V]] =
    new Format[collection.Map[K, V]] {
      private[this] val implicitKF = implicitly[Format[K]]

      override def reads(json: JsValue): JsResult[collection.Map[K, V]] = {
        val result = json.as[Map[String, V]].map {
          case (ks, v) =>
            val key = implicitKF.reads(JsString(ks)).get
            key -> v
        }
        JsSuccess(result)
      }

      override def writes(o: collection.Map[K, V]): JsValue = {
        val stringKeys = o.map { case (k, v) => implicitKF.writes(k).as[String] -> v }
        Json.toJson(stringKeys)
      }
    }.asInstanceOf[Format[Map[K, V]]]

  def customMapFormat[K, V: Format](createString: K => String, fromString: String => K): Format[Map[K, V]] =
    new Format[collection.Map[K, V]] {
      override def reads(json: JsValue): JsResult[collection.Map[K, V]] = {
        val result = json.as[Map[String, V]].map {
          case (ks, v) =>
            val key = fromString(ks)
            key -> v
        }
        JsSuccess(result)
      }

      override def writes(o: collection.Map[K, V]): JsValue = {
        val stringKeys = o.map { case (k, v) => createString(k) -> v }
        Json.toJson(stringKeys)
      }
    }.asInstanceOf[Format[Map[K, V]]]
}
