package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{Asset, Proofs}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.*
import play.api.libs.functional.syntax.*
import play.api.libs.json.*

object SignedMassTransferRequest {
  implicit val jsonFormat: Format[SignedMassTransferRequest] = Format(
    (
      (JsPath \ "version").readNullable[Byte] and
        (JsPath \ "senderPublicKey").read[String] and
        (JsPath \ "assetId").readNullable[Asset] and
        (JsPath \ "transfers").read[List[Transfer]] and
        (JsPath \ "fee").read[Long] and
        (JsPath \ "timestamp").read[Long] and
        (JsPath \ "attachment").readWithDefault(ByteStr.empty) and
        (JsPath \ "proofs").read[Proofs]
    )(SignedMassTransferRequest.apply _),
    Json.writes[SignedMassTransferRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt)))
  )
}

case class SignedMassTransferRequest(
    version: Option[Byte],
    senderPublicKey: String,
    assetId: Option[Asset],
    transfers: List[Transfer],
    fee: Long,
    timestamp: Long,
    attachment: ByteStr,
    proofs: Proofs
) {
  def toTx: Either[ValidationError, MassTransferTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _transfers <- MassTransferTransaction.parseTransfersList(transfers)
      t <- MassTransferTransaction.create(
        version.getOrElse(1.toByte),
        _sender,
        assetId.getOrElse(Asset.Waves),
        _transfers,
        fee,
        timestamp,
        attachment,
        proofs
      )
    } yield t
}
