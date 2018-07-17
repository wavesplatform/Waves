package com.wavesplatform.matcher.api

import com.wavesplatform.crypto
import com.google.common.primitives.Longs
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, JsPath, Json, Reads}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.utils.Base58
import com.wavesplatform.transaction.assets.exchange.OrderJson._

case class CancelOrderRequest(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                              @ApiModelProperty(dataType = "java.lang.String") orderIdStr: Option[String],
                              @ApiModelProperty() timestamp: Option[Long],
                              @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]) {
  @ApiModelProperty(hidden = true)
  val orderId: Option[Array[Byte]] = orderIdStr.map(id => Base58.decode(id).get)

  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = (orderId, timestamp) match {
    case (Some(orderId), _)      => senderPublicKey.publicKey ++ orderId
    case (None, Some(timestamp)) => senderPublicKey.publicKey ++ Longs.toByteArray(timestamp)
    case (None, None)            => signature // Signature can't sign itself
  }
  @ApiModelProperty(hidden = true)
  def isSignatureValid: Boolean = crypto.verify(signature, toSign, senderPublicKey.publicKey)

  def json: JsObject = Json.obj(
    "sender"    -> Base58.encode(senderPublicKey.publicKey),
    "orderId"   -> orderId.map(Base58.encode),
    "signature" -> Base58.encode(signature),
    "timestamp" -> timestamp
  )
}

object CancelOrderRequest {
  implicit val cancelOrderReads: Reads[CancelOrderRequest] = {
    val r = (JsPath \ "sender").read[PublicKeyAccount] and
      (JsPath \ "orderId").readNullable[String] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "signature").read[Array[Byte]]
    r(CancelOrderRequest.apply _)
  }
}
