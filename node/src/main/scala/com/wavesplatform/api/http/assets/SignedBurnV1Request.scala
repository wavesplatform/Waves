package com.wavesplatform.api.http.assets

import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.assets.BurnTransactionV1
import play.api.libs.functional.syntax._
import play.api.libs.json._

object SignedBurnV1Request {
  implicit val reads: Reads[SignedBurnV1Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long].orElse((JsPath \ "amount").read[Long]) and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String]
  )(SignedBurnV1Request.apply _)

  implicit val writes: Writes[SignedBurnV1Request] = Json.writes[SignedBurnV1Request]
}

case class SignedBurnV1Request(senderPublicKey: String,
                               assetId: String,
                               quantity: Long,
                               fee: Long,
                               timestamp: Long,
                               signature: String)
    extends BroadcastRequest {

  def toTx: Either[ValidationError, BurnTransactionV1] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _assetId   <- parseBase58ToAsset(assetId)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t         <- BurnTransactionV1.create(_sender, _assetId, quantity, fee, timestamp, _signature)
    } yield _t
}
