package com.wavesplatform.api.http.assets

import cats.implicits._
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.assets.BurnTransactionV2
import com.wavesplatform.transaction.Proofs
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class SignedBurnV2Request(senderPublicKey: String,
                               assetId: String,
                               quantity: Long,
                               fee: Long,
                               timestamp: Long,
                               proofs: List[String])
    extends BroadcastRequest {

  def toTx: Either[ValidationError, BurnTransactionV2] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToAsset(assetId)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      chainId = AddressScheme.current.chainId
      _t <- BurnTransactionV2.create(chainId, _sender, _assetId, quantity, fee, timestamp, _proofs)
    } yield _t
}

object SignedBurnV2Request {
  implicit val reads: Reads[SignedBurnV2Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long].orElse((JsPath \ "amount").read[Long]) and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedBurnV2Request.apply _)

  implicit val writes: Writes[SignedBurnV2Request] =
    Json.writes[SignedBurnV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))
}
