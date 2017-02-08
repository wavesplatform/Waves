package scorex.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.formats._
import scorex.crypto.encode.Base58
import scorex.transaction.assets._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidSignature

object BroadcastRequests {

  @ApiModel(value = "Signed Asset issue transaction")
  case class AssetIssueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                               sender: PublicKeyAccount,
                               @ApiModelProperty(value = "Base58 encoded name of Asset", required = true)
                               name: String,
                               @ApiModelProperty(value = "Base58 encoded description of Asset", required = true)
                               description: String,
                               @ApiModelProperty(required = true, example = "1000000")
                               quantity: Long,
                               @ApiModelProperty(allowableValues = "range[0,8]", example = "8", dataType = "integer", required = true)
                               decimals: Byte,
                               @ApiModelProperty(required = true)
                               reissuable: Boolean,
                               @ApiModelProperty(required = true)
                               fee: Long,
                               @ApiModelProperty(required = true)
                               timestamp: Long,
                               @ApiModelProperty(required = true)
                               signature: String) {

    def toTx: Either[ValidationError, IssueTransaction] =
      Base58.decode(signature).toEither.left.map(_ => InvalidSignature).flatMap { signature =>
        IssueTransaction.create(
          sender,
          name.getBytes(Charsets.UTF_8),
          description.getBytes(Charsets.UTF_8),
          quantity,
          decimals,
          reissuable,
          fee,
          timestamp,
          signature)
      }
  }


  case class AssetReissueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                 senderPublicKey: PublicKeyAccount,
                                 @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                 assetId: String,
                                 @ApiModelProperty(required = true, example = "1000000")
                                 quantity: Long,
                                 @ApiModelProperty(required = true)
                                 reissuable: Boolean,
                                 @ApiModelProperty(required = true)
                                 fee: Long,
                                 @ApiModelProperty(required = true)
                                 timestamp: Long,
                                 @ApiModelProperty(required = true)
                                 signature: String) {

    def toTx: Either[ValidationError, ReissueTransaction] = ReissueTransaction.create(
        senderPublicKey,
        Base58.decode(assetId).get,
        quantity,
        reissuable,
        fee,
        timestamp,
        Base58.decode(signature).get)
  }

  case class AssetBurnRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                senderPublicKey: String,
                              @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                assetId: String,
                              @ApiModelProperty(required = true, example = "1000000")
                                amount: Long,
                              @ApiModelProperty(required = true)
                                fee: Long,
                              @ApiModelProperty(required = true)
                                timestamp: Long,
                              @ApiModelProperty(required = true)
                                signature: String) {

    def toTx: Either[ValidationError, BurnTransaction] = BurnTransaction.create(
        new PublicKeyAccount(Base58.decode(senderPublicKey).get),
        Base58.decode(assetId).get,
        amount,
        fee,
        timestamp,
        Base58.decode(signature).get)
  }

  @ApiModel(value = "Signed Asset transfer transaction")
  case class AssetTransferRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                  sender: PublicKeyAccount,
                                  @ApiModelProperty(value = "Base58 encoded Asset ID")
                                  assetId: Option[String],
                                  @ApiModelProperty(value = "Recipient address", required = true)
                                  recipient: Account,
                                  @ApiModelProperty(required = true, example = "1000000")
                                  amount: Long,
                                  @ApiModelProperty(required = true)
                                  fee: Long,
                                  @ApiModelProperty(value = "Fee asset ID")
                                  feeAssetId: Option[String],
                                  @ApiModelProperty(required = true)
                                  timestamp: Long,
                                  @ApiModelProperty(value = "Base58 encoded attachment")
                                  attachment: Option[String],
                                  @ApiModelProperty(required = true)
                                  signature: String) {
    def toTx: Either[ValidationError, TransferTransaction] =
      TransferTransaction.create(
        assetId.map(Base58.decode(_).get),
        sender,
        recipient,
        amount,
        timestamp,
        feeAssetId.map(Base58.decode(_).get),
        fee,
        attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        Base58.decode(signature).get)
  }

  implicit val assetTransferRequestFormat: Format[AssetTransferRequest] = (
    (__ \ "senderPublicKey").format[PublicKeyAccount] ~
      (__ \ "assetId").formatNullable[String] ~
      (__ \ "recipient").format[Account] ~
      (__ \ "amount").format[Long] ~
      (__ \ "fee").format[Long] ~
      (__ \ "feeAssetId").formatNullable[String] ~
      (__ \ "timestamp").format[Long] ~
      (__ \ "attachment").formatNullable[String] ~
      (__ \ "signature").format[String]
    ) (AssetTransferRequest.apply, unlift(AssetTransferRequest.unapply))

  implicit val assetIssueRequestReads: Format[AssetIssueRequest] = (
    (__ \ "senderPublicKey").format[PublicKeyAccount] ~
      (__ \ "name").format[String] ~
      (__ \ "description").format[String] ~
      (__ \ "quantity").format[Long] ~
      (__ \ "decimals").format[Byte] ~
      (__ \ "reissuable").format[Boolean] ~
      (__ \ "fee").format[Long] ~
      (__ \ "timestamp").format[Long] ~
      (__ \ "signature").format[String](SignatureFormat)
    ) (AssetIssueRequest.apply, unlift(AssetIssueRequest.unapply))

  implicit val assetReissueRequestReads: Format[AssetReissueRequest] = (
    (__ \ "senderPublicKey").format[PublicKeyAccount] ~
      (__ \ "assetId").format[String] ~
      (__ \ "quantity").format[Long] ~
      (__ \ "reissuable").format[Boolean] ~
      (__ \ "fee").format[Long] ~
      (__ \ "timestamp").format[Long] ~
      (__ \ "signature").format[String](SignatureFormat)
    ) (AssetReissueRequest.apply, unlift(AssetReissueRequest.unapply))

  implicit val assetBurnRequestReads: Format[AssetBurnRequest] = (
    (__ \ "senderPublicKey").format[String] ~
      (__ \ "assetId").format[String] ~
      (__ \ "quantity").format[Long] ~
      (__ \ "fee").format[Long] ~
      (__ \ "timestamp").format[Long] ~
      (__ \ "signature").format[String]
    ) (AssetBurnRequest.apply, unlift(AssetBurnRequest.unapply))
}
