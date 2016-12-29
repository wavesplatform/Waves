package scorex.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.formats._
import scorex.crypto.encode.Base58
import scorex.transaction.assets._

import scala.util.Try

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

    def toTx: Try[IssueTransaction] = Try {
      IssueTransaction.create(
        sender,
        name.getBytes(Charsets.UTF_8),
        description.getBytes(Charsets.UTF_8),
        quantity,
        decimals,
        reissuable,
        fee,
        timestamp,
        Base58.decode(signature).get
      ).right.get
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

    def toTx: Try[ReissueTransaction] = Try {
      ReissueTransaction.create(
        senderPublicKey,
        Base58.decode(assetId).get,
        quantity,
        reissuable,
        fee,
        timestamp,
        Base58.decode(signature).get).right.get
    }
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

    def toTx: Try[BurnTransaction] = Try {
      BurnTransaction.create(
        new PublicKeyAccount(Base58.decode(senderPublicKey).get),
        Base58.decode(assetId).get,
        amount,
        fee,
        timestamp,
        Base58.decode(signature).get).right.get
    }
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
    def toTx: Try[TransferTransaction] = Try {
      TransferTransaction.create(
        assetId.map(Base58.decode(_).get),
        sender,
        recipient,
        amount,
        timestamp,
        feeAssetId.map(_.getBytes),
        fee,
        attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
        Base58.decode(signature).get).right.get
    }
  }

  implicit val assetTransferRequestReads: Reads[AssetTransferRequest] = (
    (JsPath \ "senderPublicKey").read[PublicKeyAccount] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "recipient").read[Account] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "feeAssetId").readNullable[String] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (AssetTransferRequest.apply _)

  implicit val assetIssueRequestReads: Reads[AssetIssueRequest] = (
    (JsPath \ "senderPublicKey").read[PublicKeyAccount] and
      (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "decimals").read[Byte] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (AssetIssueRequest.apply _)

  implicit val assetReissueRequestReads: Reads[AssetReissueRequest] = (
    (JsPath \ "senderPublicKey").read[PublicKeyAccount] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (AssetReissueRequest.apply _)

  //TODO put reads/writes together?
  implicit val assetBurnRequestReads: Reads[AssetBurnRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String]
    ) (AssetBurnRequest.apply _)
}

