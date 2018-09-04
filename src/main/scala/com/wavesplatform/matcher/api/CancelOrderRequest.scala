package com.wavesplatform.matcher.api

import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import com.wavesplatform.utils.Base58
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.PublicKeyAccount

case class CancelOrderRequest(@ApiModelProperty(dataType = "java.lang.String") sender: PublicKeyAccount,
                              @ApiModelProperty(dataType = "java.lang.String") orderId: ByteStr,
                              @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]) {
  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = sender.publicKey ++ orderId.arr

  @ApiModelProperty(hidden = true)
  val isSignatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature, toSign, sender.publicKey))
}

object CancelOrderRequest {
  implicit val byteArrayFormat: Format[Array[Byte]] = Format(
    {
      case JsString(base58String) => Base58.decode(base58String).fold(_ => JsError("Invalid signature"), b => JsSuccess(b))
      case other                  => JsError(s"Expecting string but got $other")
    },
    b => JsString(Base58.encode(b))
  )

  implicit val pkFormat: Format[PublicKeyAccount] = Format(
    {
      case JsString(value) => PublicKeyAccount.fromBase58String(value).fold(_ => JsError("Invalid public key"), pk => JsSuccess(pk))
      case other           => JsError(s"Expecting string but got $other")
    },
    pk => JsString(Base58.encode(pk.publicKey))
  )

  implicit val format: OFormat[CancelOrderRequest] = Json.format
}
