package com.wavesplatform.matcher.api

import com.google.common.primitives.Longs
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.OrderJson._
import com.wavesplatform.utils.Base58
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsObject, JsPath, Json, Reads}

case class CancelOrderRequest(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                              @ApiModelProperty(dataType = "java.lang.String") orderId: Option[ByteStr],
                              @ApiModelProperty() timestamp: Option[Long],
                              @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]) {

  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = (orderId, timestamp) match {
    case (Some(oid), _)   => senderPublicKey.publicKey ++ oid.arr
    case (None, Some(ts)) => senderPublicKey.publicKey ++ Longs.toByteArray(ts)
    case (None, None)     => signature // Signature can't sign itself
  }
  @ApiModelProperty(hidden = true)
  val isSignatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature, toSign, senderPublicKey.publicKey))

  def json: JsObject = Json.obj(
    "sender"    -> Base58.encode(senderPublicKey.publicKey),
    "orderId"   -> orderId.map(_.base58),
    "signature" -> Base58.encode(signature),
    "timestamp" -> timestamp
  )
}

object CancelOrderRequest {
  implicit val cancelOrderReads: Reads[CancelOrderRequest] = {
    val r = (JsPath \ "sender").read[PublicKeyAccount] and
      (JsPath \ "orderId").readNullable[ByteStr] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "signature").read[Array[Byte]]
    r(CancelOrderRequest.apply _)
  }
}
