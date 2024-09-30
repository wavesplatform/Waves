package com.wavesplatform.api.http.requests

import cats.instances.list.*
import cats.syntax.traverse.*
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.functional.syntax.*
import play.api.libs.json.*

object SignedIssueV2Request {
  implicit val signedExchangeRequestReads: Reads[SignedIssueV2Request] = {
    (
      (JsPath \ "senderPublicKey").read[String] and
        (JsPath \ "name").read[String] and
        (JsPath \ "description").read[String] and
        (JsPath \ "quantity").read[Long] and
        (JsPath \ "decimals").read[Byte] and
        (JsPath \ "reissuable").read[Boolean] and
        (JsPath \ "fee").read[Long] and
        (JsPath \ "timestamp").read[Long] and
        (JsPath \ "proofs").read[List[ProofStr]] and
        (JsPath \ "script").readNullable[String]
    )(SignedIssueV2Request.apply _)
  }
  implicit val writes: Writes[SignedIssueV2Request] =
    Json
      .writes[SignedIssueV2Request]
      .transform((request: JsObject) =>
        request + ("version" -> JsNumber(2))
          + ("type"          -> JsNumber(IssueTransaction.typeId.toInt))
      )
}

case class SignedIssueV2Request(
    senderPublicKey: String,
    name: String,
    description: String,
    quantity: Long,
    decimals: Byte,
    reissuable: Boolean,
    fee: Long,
    timestamp: Long,
    proofs: List[String],
    script: Option[String]
) {
  def toTx: Either[ValidationError, IssueTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _script <- script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      t <- IssueTransaction.create(
        TxVersion.V2,
        _sender,
        name,
        description,
        quantity,
        decimals,
        reissuable,
        _script,
        fee,
        timestamp,
        _proofs
      )
    } yield t
}
