package com.wavesplatform.matcher.api

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, JsPath, Json, Reads}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.OrderJson._

case class CancelOrderRequest(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                              @ApiModelProperty(dataType = "java.lang.String") orderId: Array[Byte],
                              @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]) {
  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = senderPublicKey.publicKey ++ orderId

  @ApiModelProperty(hidden = true)
  def isSignatureValid = EllipticCurveImpl.verify(signature, toSign, senderPublicKey.publicKey)

  def json: JsObject = Json.obj(
    "sender" -> Base58.encode(senderPublicKey.publicKey),
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
