package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.BurnTransaction
import play.api.libs.json.{Format, Json}

case class BurnRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    assetId: String,
    quantity: Long,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[String],
    proofs: Option[List[String]]
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, BurnTransaction] =
    for {
      validProofs <- toProofs(version, signature, proofs)
      validAsset  <- toAsset(Some(assetId))
      tx <- BurnTransaction.create(
        version.getOrElse(defaultVersion),
        sender,
        validAsset.asInstanceOf[IssuedAsset],
        quantity,
        fee,
        timestamp.getOrElse(defaultTimestamp),
        validProofs
      )
    } yield tx
}

object BurnRequest {
  implicit val jsonFormat = Format(
    {
      import play.api.libs.functional.syntax._
      import play.api.libs.json._
      ((JsPath \ "version").readNullable[Byte] and
        (JsPath \ "sender").readNullable[String] and
        (JsPath \ "senderPublicKey").readNullable[String] and
        (JsPath \ "assetId").read[String] and
        (JsPath \ "quantity").read[Long].orElse((JsPath \ "amount").read[Long]) and
        (JsPath \ "fee").read[Long] and
        (JsPath \ "timestamp").readNullable[Long] and
        (JsPath \ "signature").readNullable[String] and
        (JsPath \ "proofs").readNullable[List[String]])(BurnRequest.apply _)
    },
    Json.writes[BurnRequest]
  )
}
