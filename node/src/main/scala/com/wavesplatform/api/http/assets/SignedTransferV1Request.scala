package com.wavesplatform.api.http.assets

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.transfer._
import play.api.libs.functional.syntax._
import play.api.libs.json._

object SignedTransferV1Request {
  implicit val reads: Reads[SignedTransferV1Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "feeAssetId").read[String].map(Option.apply).orElse((JsPath \ "feeAsset").readNullable[String]) and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "signature").read[String]
  )(SignedTransferV1Request.apply _)

  implicit val writes: Writes[SignedTransferV1Request] = Json.writes[SignedTransferV1Request]
}

case class SignedTransferV1Request(senderPublicKey: String,
                                   assetId: Option[String],
                                   recipient: String,
                                   amount: Long,
                                   fee: Long,
                                   feeAssetId: Option[String],
                                   timestamp: Long,
                                   attachment: Option[String],
                                   signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, TransferTransactionV1] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToAssetId(assetId, "invalid.assetId") //parseBase58ToOption(assetId.filter(_.length > 0), "invalid.assetId", transaction.AssetIdStringLength).map(AssetId.fromCompatId)
      _feeAssetId <- parseBase58ToAssetId(feeAssetId, "invalid.feeAssetId") //parseBase58ToOption(feeAssetId.filter(_.length > 0), "invalid.feeAssetId", transaction.AssetIdStringLength).map(AssetId.fromCompatId)
      _signature  <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      _account    <- AddressOrAlias.fromString(recipient)
      t           <- TransferTransactionV1.create(_assetId, _sender, _account, amount, timestamp, _feeAssetId, fee, _attachment.arr, _signature)
    } yield t
}
