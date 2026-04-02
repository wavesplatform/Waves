package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.SetScriptTransaction
import play.api.libs.functional.syntax.*
import play.api.libs.json.*

object SetScriptRequest {
  given Reads[SetScriptRequest] = (
    (JsPath \ "version").readNullable[Byte] and
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "script").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").readWithDefault(Proofs.empty)
  )(SetScriptRequest.apply)

  implicit val signedSetScriptRequestWrites: OWrites[SetScriptRequest] =
    Json.writes[SetScriptRequest].transform((request: JsObject) => request + ("version" -> JsNumber(1)))
}

case class SetScriptRequest(
    version: Option[Byte],
    senderPublicKey: String,
    script: Option[String],
    fee: Long,
    timestamp: Long,
    proofs: Proofs
) extends TxBroadcastRequest[SetScriptTransaction] {
  def toTx: Either[ValidationError, SetScriptTransaction] =
    for {
      _sender <- PublicKey.fromBase58String(senderPublicKey)
      _script <- script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      t <- SetScriptTransaction.create(version.getOrElse(1.toByte), _sender, _script, fee, timestamp, proofs)
    } yield t
}
