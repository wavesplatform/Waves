package com.wavesplatform.matcher.api

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, JsPath, Json, Reads}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.OrderJson._

case class CancelOrderRequest(sender: PublicKeyAccount, orderId: Array[Byte], signature: Array[Byte]) {
  lazy val toSign: Array[Byte] = sender.publicKey ++ orderId

  def isSignatureValid = EllipticCurveImpl.verify(signature, toSign, sender.publicKey)

  def json: JsObject = Json.obj(
    "sender" -> Base58.encode(sender.publicKey),
    "orderId" -> Base58.encode(orderId),
    "signature" -> Base58.encode(signature)
  )
}

object CancelOrderRequest {
  implicit val cancelOrderReads: Reads[CancelOrderRequest] = {
    val r = (JsPath \ "sender").read[PublicKeyAccount] and
      (JsPath \ "orderId").read[Array[Byte]] and
      (JsPath \ "signature").read[Array[Byte]]
    r(CancelOrderRequest.apply _)
  }

  def sign(unsigned: CancelOrderRequest, sender: PrivateKeyAccount): CancelOrderRequest = {
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }
}
