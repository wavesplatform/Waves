package com.wavesplatform.matcher.api

import com.wavesplatform.crypto
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, JsPath, Json, Reads}
import scorex.account.PublicKeyAccount
import com.wavesplatform.utils.Base58
import monix.eval.Coeval
import scorex.transaction.assets.exchange.OrderJson._

case class CancelOrderRequest(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                              @ApiModelProperty(dataType = "java.lang.String") orderId: Array[Byte],
                              @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]) {
  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = senderPublicKey.publicKey ++ orderId

  @ApiModelProperty(hidden = true)
  val isSignatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature, toSign, senderPublicKey.publicKey))

  def json: JsObject = Json.obj(
    "sender"    -> Base58.encode(senderPublicKey.publicKey),
    "orderId"   -> Base58.encode(orderId),
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
}
