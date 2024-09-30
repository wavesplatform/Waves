package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.BurnTransaction
import play.api.libs.functional.syntax.*
import play.api.libs.json.*

case class BurnRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    asset: IssuedAsset,
    quantity: Long,
    fee: Long,
    timestamp: Option[Long],
    signature: Option[ByteStr],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[BurnTransaction] {
  def toTxFrom(sender: PublicKey): Either[ValidationError, BurnTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      tx <- BurnTransaction.create(
        version.getOrElse(defaultVersion),
        sender,
        asset,
        quantity,
        fee,
        timestamp.getOrElse(defaultTimestamp),
        validProofs
      )
    } yield tx
}

object BurnRequest {
  import com.wavesplatform.utils.byteStrFormat
  implicit val jsonFormat: Format[BurnRequest] = Format(
    ((JsPath \ "version").readNullable[Byte] and
      (JsPath \ "sender").readNullable[String] and
      (JsPath \ "senderPublicKey").readNullable[String] and
      (JsPath \ "assetId").read[IssuedAsset] and
      (JsPath \ "amount").read[Long].orElse((JsPath \ "quantity").read[Long]) and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").readNullable[Long] and
      (JsPath \ "signature").readNullable[ByteStr] and
      (JsPath \ "proofs").readNullable[Proofs])(BurnRequest.apply _),
    Json.writes[BurnRequest]
  )
}
