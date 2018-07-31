package com.wavesplatform.matcher.api

import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import com.wavesplatform.utils.Base58
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, JsPath, Json, Reads}
import scorex.account.PublicKeyAccount
import scorex.transaction.assets.exchange.OrderJson._

case class CancelOrderRequest(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                              @ApiModelProperty(dataType = "java.lang.String") orderId: ByteStr,
                              @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]) {
  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = senderPublicKey.publicKey ++ orderId.arr

  @ApiModelProperty(hidden = true)
  val isSignatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature, toSign, senderPublicKey.publicKey))

  def json: JsObject = Json.obj(
    "sender"    -> Base58.encode(senderPublicKey.publicKey),
    "orderId"   -> orderId.base58,
    "signature" -> Base58.encode(signature)
  )
}

object CancelOrderRequest {
  implicit val cancelOrderReads: Reads[CancelOrderRequest] = {
    val r = (JsPath \ "sender").read[PublicKeyAccount] and
      (JsPath \ "orderId").read[ByteStr] and
      (JsPath \ "signature").read[Array[Byte]]
    r(CancelOrderRequest.apply _)
  }
}
