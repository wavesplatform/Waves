package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.BurnTransaction
import play.api.libs.functional.syntax.*
import play.api.libs.json.*

object SignedBurnV1Request {
  implicit val reads: Reads[SignedBurnV1Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String]
  )(SignedBurnV1Request.apply _)

  implicit val writes: Writes[SignedBurnV1Request] = Json.writes[SignedBurnV1Request]
}

case class SignedBurnV1Request(
    senderPublicKey: String,
    assetId: String,
    amount: Long,
    fee: Long,
    timestamp: Long,
    signature: String
) {

  def toTx: Either[ValidationError, BurnTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _assetId   <- parseBase58ToIssuedAsset(assetId)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t         <- BurnTransaction.create(1.toByte, _sender, _assetId, amount, fee, timestamp, Proofs(_signature))
    } yield _t
}
