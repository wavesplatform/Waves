package com.wavesplatform.matcher.model

import scala.util.{Failure, Success}

import play.api.data.validation.ValidationError
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.crypto.encode.Base58

object OrderConverters {
  implicit val byteArrayReads = new Reads[Array[Byte]] {
    def reads(json: JsValue) = json match {
      case JsString(s) => Base58.decode(s) match {
        case Success(bytes) => JsSuccess(bytes)
        case Failure(_) => JsError(Seq(JsPath() -> Seq(ValidationError("error.incorrect.base58"))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
  }

  implicit val publicKeyAccountReads = new Reads[PublicKeyAccount] {
    def reads(json: JsValue) = json match {
      case JsString(s) => Base58.decode(s) match {
        case Success(bytes) if bytes.length == 32 => JsSuccess(new PublicKeyAccount(bytes))
        case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.incorrect.publicKeyAccount"))))
      }
      case _ => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.jsstring"))))
    }
  }
}
