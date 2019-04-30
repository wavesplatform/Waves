package com.wavesplatform.api.http

import play.api.libs.json.{JsPath, Reads}
import play.api.libs.functional.syntax._

case class SignedMessage(message: String, signature: String, publickey: String)

object SignedMessage {

  implicit val messageReads: Reads[SignedMessage] = (
    (JsPath \ "message").read[String] and
      (JsPath \ "signature").read[String] and
      (JsPath \ "publickey")
        .read[String]
        .orElse((JsPath \ "publicKey").read[String])
  )(SignedMessage.apply _)

}
