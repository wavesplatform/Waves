package com.wavesplatform.api.http.requests

import cats.syntax.traverse.*
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.BurnTransaction
import play.api.libs.functional.syntax.*
import play.api.libs.json.*

case class SignedBurnV2Request(
    senderPublicKey: String,
    assetId: String,
    amount: Long,
    fee: Long,
    timestamp: Long,
    proofs: List[String]
) {

  def toTx: Either[ValidationError, BurnTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToIssuedAsset(assetId)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- BurnTransaction.create(2.toByte, _sender, _assetId, amount, fee, timestamp, _proofs)
    } yield _t
}

object SignedBurnV2Request {
  implicit val reads: Reads[SignedBurnV2Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedBurnV2Request.apply _)

  implicit val writes: Writes[SignedBurnV2Request] =
    Json.writes[SignedBurnV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))
}
