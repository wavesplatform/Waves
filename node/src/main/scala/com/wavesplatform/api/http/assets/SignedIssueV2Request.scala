package com.wavesplatform.api.http.assets

import cats.implicits._
import com.google.common.base.Charsets
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV2}
import com.wavesplatform.transaction.Proofs
import play.api.libs.functional.syntax._
import play.api.libs.json._

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
      .transform(
        (request: JsObject) =>
          request + ("version" -> JsNumber(2))
            + ("type"          -> JsNumber(IssueTransaction.typeId.toInt)))
}

case class SignedIssueV2Request(senderPublicKey: String,
                                name: String,
                                description: String,
                                quantity: Long,
                                decimals: Byte,
                                reissuable: Boolean,
                                fee: Long,
                                timestamp: Long,
                                proofs: List[String],
                                script: Option[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, IssueTransactionV2] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _script <- script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      t <- IssueTransactionV2.create(
        AddressScheme.current.chainId,
        _sender,
        name.getBytes(Charsets.UTF_8),
        description.getBytes(Charsets.UTF_8),
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
