package scorex.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import scorex.account.{AddressOrAlias, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import scorex.transaction.assets.{TransferTransaction, VersionedTransferTransaction}
import scorex.transaction.{AssetIdStringLength, Proofs, ValidationError}

object SignedVersionedTransferRequest {
  implicit val jsonFormat: Format[SignedVersionedTransferRequest] = Json.format
}

@ApiModel(value = "Signed Asset transfer transaction")
case class SignedVersionedTransferRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                          senderPublicKey: String,
                                          @ApiModelProperty(value = "Base58 encoded Asset ID")
                                          assetId: Option[String],
                                          @ApiModelProperty(value = "Recipient address", required = true)
                                          recipient: String,
                                          @ApiModelProperty(required = true, example = "1000000")
                                          amount: Long,
                                          @ApiModelProperty(required = true)
                                          fee: Long,
                                          @ApiModelProperty(required = true)
                                          timestamp: Long,
                                          @ApiModelProperty(required = true)
                                          version: Int,
                                          @ApiModelProperty(value = "Base58 encoded attachment")
                                          attachment: Option[String],
                                          @ApiModelProperty(required = true)
                                          proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, VersionedTransferTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToOption(assetId.filter(_.length > 0), "invalid.assetId", AssetIdStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _recipient <- AddressOrAlias.fromString(recipient)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      t           <- VersionedTransferTransaction.create(2, _assetId, _sender,_recipient, amount, timestamp, fee, _attachment.arr, _proofs)
    } yield t
}
