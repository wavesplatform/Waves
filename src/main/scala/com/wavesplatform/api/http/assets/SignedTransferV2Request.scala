package com.wavesplatform.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Json, OFormat}
import com.wavesplatform.account.{AddressOrAlias, PublicKeyAccount}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs, ValidationError}

object SignedTransferV2Request {
  implicit val format: OFormat[SignedTransferV2Request] = Json.format
}

@ApiModel(value = "Signed Asset transfer transaction")
case class SignedTransferV2Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                   senderPublicKey: String,
                                   @ApiModelProperty(value = "Base58 encoded Asset ID")
                                   assetId: Option[String],
                                   @ApiModelProperty(value = "Recipient address", required = true)
                                   recipient: String,
                                   @ApiModelProperty(required = true, example = "1000000")
                                   amount: Long,
                                   @ApiModelProperty(value = "Base58 encoded Fee Asset ID")
                                   feeAssetId: Option[String],
                                   @ApiModelProperty(required = true)
                                   fee: Long,
                                   @ApiModelProperty(required = true)
                                   timestamp: Long,
                                   @ApiModelProperty(required = true)
                                   version: Byte,
                                   @ApiModelProperty(value = "Base58 encoded attachment")
                                   attachment: Option[String],
                                   @ApiModelProperty(required = true)
                                   proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, TransferTransactionV2] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToOption(assetId.filter(_.length > 0), "invalid.assetId", AssetIdStringLength)
      _feeAssetId <- parseBase58ToOption(feeAssetId.filter(_.length > 0), "invalid.assetId", AssetIdStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _recipient  <- AddressOrAlias.fromString(recipient)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      t           <- TransferTransactionV2.create(version, _assetId, _sender, _recipient, amount, timestamp, _feeAssetId, fee, _attachment.arr, _proofs)
    } yield t
}
