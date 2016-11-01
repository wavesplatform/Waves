package scorex.api.http.assets

import io.swagger.annotations._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.{IssueTransaction, ReissueTransaction, TransferTransaction}

import scala.util.Try

object BroadcastRequests {

  @ApiModel(value = "Signed Asset issue transaction")
  case class AssetIssueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                               senderPublicKey: String,
                               @ApiModelProperty(value = "Base58 encoded name of Asset", required = true)
                               name: String,
                               @ApiModelProperty(value = "Base58 encoded description of Asset", required = true)
                               description: String,
                               @ApiModelProperty(required = true, example = "1000000")
                               quantity: Long,
                               @ApiModelProperty(allowableValues = "range[0,8]", example = "2", dataType = "integer", required = true)
                               decimals: Byte,
                               @ApiModelProperty(required = true)
                               reissuable: Boolean,
                               @ApiModelProperty(required = true)
                               fee: Long,
                               @ApiModelProperty(required = true)
                               timestamp: Long,
                               @ApiModelProperty(required = true)
                               signature: String) {

    def toTx: Try[IssueTransaction] = Try {
      IssueTransaction(
        new PublicKeyAccount(Base58.decode(senderPublicKey).get),
        Base58.decode(name).get,
        Base58.decode(description).get,
        quantity,
        decimals,
        reissuable,
        fee,
        timestamp,
        Base58.decode(signature).get
      )
    }
  }


  case class AssetReissueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                 senderPublicKey: String,
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

    def toTx: Try[ReissueTransaction] = Try {
      ReissueTransaction(
        new PublicKeyAccount(Base58.decode(senderPublicKey).get),
        Base58.decode(assetId).get,
        quantity,
        reissuable,
        fee,
        timestamp,
        Base58.decode(signature).get)
    }
  }

  @ApiModel(value = "Signed Asset transfer transaction")
  case class AssetTransferRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
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
                                  @ApiModelProperty(value = "Base58 encoded attachment")
                                  attachment: Option[String],
                                  @ApiModelProperty(required = true)
                                  signature: String) {
    def toTx: Try[TransferTransaction] = Try {
      TransferTransaction(
        assetId.map(Base58.decode(_).get),
        new PublicKeyAccount(Base58.decode(senderPublicKey).get),
        new Account(recipient),
        amount,
        timestamp,
        None,
        fee,
        attachment.map(Base58.decode(_).get).getOrElse(new Array[Byte](0)),
        Base58.decode(signature).get)
    }
  }

  implicit val assetTransferRequestReads: Reads[AssetTransferRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "signature").read[String]
    ) (AssetTransferRequest.apply _)

  implicit val assetIssueRequestReads: Reads[AssetIssueRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "decimals").read[Byte] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String]
    ) (AssetIssueRequest.apply _)

  implicit val assetReissueRequestReads: Reads[AssetReissueRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String]
    ) (AssetReissueRequest.apply _)
}

