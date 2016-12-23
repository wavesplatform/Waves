package scorex.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Writes}
import scorex.crypto.encode.Base58
import scorex.transaction.assets._

/**
  */
object BroadcastResponses {

  @ApiModel(value = "Asset issue transaction")
  case class AssetIssueResponse(@ApiModelProperty(value = "Transaction ID", required = true)
                                id: String,
                                @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                assetId: String,
                                @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                senderPublicKey: String,
                                @ApiModelProperty(value = "Base58 encoded name of Asset", required = true)
                                name: String,
                                @ApiModelProperty(value = "Base58 encoded description of Asset", required = true)
                                description: String,
                                @ApiModelProperty(required = true)
                                quantity: Long,
                                @ApiModelProperty(dataType = "integer", required = true)
                                decimals: Byte,
                                @ApiModelProperty(required = true)
                                reissuable: Boolean,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                @ApiModelProperty(required = true)
                                timestamp: Long,
                                @ApiModelProperty(required = true)
                                signature: String) {
  }

  object AssetIssueResponse {
    def apply(tx: IssueTransaction): AssetIssueResponse = new AssetIssueResponse(
      Base58.encode(tx.id),
      Base58.encode(tx.assetId),
      Base58.encode(tx.sender.publicKey),
      new String(tx.name, Charsets.UTF_8),
      new String(tx.description, Charsets.UTF_8),
      tx.quantity, tx.decimals, tx.reissuable, tx.fee, tx.timestamp,
      Base58.encode(tx.signature)
    )

    implicit val issueResponseWrites: Writes[AssetIssueResponse] = (
      (JsPath \ "id").write[String] and
        (JsPath \ "assetId").write[String] and
        (JsPath \ "senderPublicKey").write[String] and
        (JsPath \ "name").write[String] and
        (JsPath \ "description").write[String] and
        (JsPath \ "quantity").write[Long] and
        (JsPath \ "decimals").write[Byte] and
        (JsPath \ "reissuable").write[Boolean] and
        (JsPath \ "fee").write[Long] and
        (JsPath \ "timestamp").write[Long] and
        (JsPath \ "signature").write[String]
      ) (unlift(AssetIssueResponse.unapply))
  }

  @ApiModel(value = "Asset reissue transaction")
  case class AssetReissueResponse(@ApiModelProperty(value = "Transaction ID", required = true)
                                  id: String,
                                  @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                  assetId: String,
                                  @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                  senderPublicKey: String,
                                  @ApiModelProperty(required = true)
                                  quantity: Long,
                                  @ApiModelProperty(required = true)
                                  reissuable: Boolean,
                                  @ApiModelProperty(required = true)
                                  fee: Long,
                                  @ApiModelProperty(required = true)
                                  timestamp: Long,
                                  @ApiModelProperty(required = true)
                                  signature: String) {
  }

  object AssetReissueResponse {
    def apply(tx: ReissueTransaction): AssetReissueResponse = new AssetReissueResponse(
      Base58.encode(tx.id),
      Base58.encode(tx.assetId),
      Base58.encode(tx.sender.publicKey),
      tx.quantity,
      tx.reissuable,
      tx.fee,
      tx.timestamp,
      Base58.encode(tx.signature)
    )

    implicit val reissueResponseWrites: Writes[AssetReissueResponse] = (
      (JsPath \ "id").write[String] and
        (JsPath \ "assetId").write[String] and
        (JsPath \ "senderPublicKey").write[String] and
        (JsPath \ "quantity").write[Long] and
        (JsPath \ "reissuable").write[Boolean] and
        (JsPath \ "fee").write[Long] and
        (JsPath \ "timestamp").write[Long] and
        (JsPath \ "signature").write[String]
      ) (unlift(AssetReissueResponse.unapply))
  }

  @ApiModel(value = "Asset burn transaction")
  case class AssetBurnResponse(@ApiModelProperty(value = "Transaction ID", required = true)
                               id: String,
                               @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                               assetId: String,
                               @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                               senderPublicKey: String,
                               @ApiModelProperty(required = true)
                               quantity: Long,
                               @ApiModelProperty(required = true)
                               fee: Long,
                               @ApiModelProperty(required = true)
                               timestamp: Long,
                               @ApiModelProperty(required = true)
                               signature: String) {
  }

  object AssetBurnResponse {
    def apply(tx: BurnTransaction): AssetBurnResponse = new AssetBurnResponse(
      Base58.encode(tx.id),
      Base58.encode(tx.assetId),
      Base58.encode(tx.sender.publicKey),
      tx.amount,
      tx.fee,
      tx.timestamp,
      Base58.encode(tx.signature)
    )

    implicit val reissueResponseWrites: Writes[AssetBurnResponse] = (
      (JsPath \ "id").write[String] and
        (JsPath \ "assetId").write[String] and
        (JsPath \ "senderPublicKey").write[String] and
        (JsPath \ "quantity").write[Long] and
        (JsPath \ "fee").write[Long] and
        (JsPath \ "timestamp").write[Long] and
        (JsPath \ "signature").write[String]
      ) (unlift(AssetBurnResponse.unapply))
  }


  @ApiModel(value = "Asset transfer transaction")
  case class AssetTransferResponse(@ApiModelProperty(value = "Transaction ID", required = true)
                                   id: String,
                                   @ApiModelProperty(value = "Base58 encoded Asset ID")
                                   assetId: Option[String],
                                   @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                   senderPublicKey: String,
                                   @ApiModelProperty(value = "Recipient address", required = true)
                                   recipient: String,
                                   @ApiModelProperty(required = true)
                                   amount: Long,
                                   @ApiModelProperty(required = true)
                                   fee: Long,
                                   @ApiModelProperty(required = true)
                                   timestamp: Long,
                                   @ApiModelProperty
                                   attachment: Option[String],
                                   @ApiModelProperty(required = true)
                                   signature: String) {
  }

  object AssetTransferResponse {
    def apply(tx: TransferTransaction): AssetTransferResponse = new AssetTransferResponse(
      Base58.encode(tx.id),
      tx.assetId.map(Base58.encode),
      Base58.encode(tx.sender.publicKey),
      tx.recipient.address,
      tx.amount,
      tx.fee,
      tx.timestamp,
      if (tx.attachment.length > 0) Some(Base58.encode(tx.attachment)) else None,
      Base58.encode(tx.signature)
    )

    implicit val transferResponseWrites: Writes[AssetTransferResponse] = (
      (JsPath \ "id").write[String] and
        (JsPath \ "assetId").writeNullable[String] and
        (JsPath \ "senderPublicKey").write[String] and
        (JsPath \ "recipient").write[String] and
        (JsPath \ "amount").write[Long] and
        (JsPath \ "fee").write[Long] and
        (JsPath \ "timestamp").write[Long] and
        (JsPath \ "attachment").writeNullable[String] and
        (JsPath \ "signature").write[String]
      ) (unlift(AssetTransferResponse.unapply))
  }

}
