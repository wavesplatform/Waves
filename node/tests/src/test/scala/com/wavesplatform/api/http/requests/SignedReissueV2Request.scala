package com.wavesplatform.api.http.requests

import cats.instances.list.*
import cats.syntax.traverse.*
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.ReissueTransaction
import play.api.libs.functional.syntax.*
import play.api.libs.json.{JsPath, Reads}

case class SignedReissueV2Request(
    senderPublicKey: String,
    assetId: String,
    quantity: Long,
    reissuable: Boolean,
    fee: Long,
    timestamp: Long,
    proofs: List[String]
) {
  def toTx: Either[ValidationError, ReissueTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _assetId    <- parseBase58ToIssuedAsset(assetId)
      _t          <- ReissueTransaction.create(2.toByte, _sender, _assetId, quantity, reissuable, fee, timestamp, _proofs)
    } yield _t
}

object SignedReissueV2Request {
  implicit val assetReissueRequestReads: Reads[SignedReissueV2Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedReissueV2Request.apply _)
}
