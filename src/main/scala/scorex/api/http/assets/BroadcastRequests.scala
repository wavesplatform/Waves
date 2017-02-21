package scorex.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.TypedTransaction._
import scorex.transaction.{AssetIdStringLength, ValidationError}
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.assets._

object BroadcastRequests {
  private def parseBase58(v: String, error: String, maxLength: Int): Either[ValidationError, Array[Byte]] =
    if (v.length > maxLength) Left(ValidationError.CustomValidationError(error))
    else Base58.decode(v).toOption.toRight(ValidationError.CustomValidationError(error))

  private def parseBase58(v: Option[String], error: String, maxLength: Int): Either[ValidationError, Array[Byte]] =
    v.fold[Either[ValidationError, Array[Byte]]](Right(Array.emptyByteArray))(_v => parseBase58(_v, error, maxLength))

  private def parseBase58ToOption(v: Option[String], error: String, maxLength: Int): Either[ValidationError, Option[Array[Byte]]] =
    v.fold[Either[ValidationError, Option[Array[Byte]]]](Right(None)) { s => parseBase58(s, error, maxLength).map(b => Option(b)) }

  @ApiModel(value = "Signed Asset issue transaction")
  case class AssetIssueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                               senderPublicKey: String,
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

    def toTx: Either[ValidationError, IssueTransaction] = for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid signature", SignatureStringLength)
      _t <- IssueTransaction.create(_sender, name.getBytes(Charsets.UTF_8), description.getBytes(Charsets.UTF_8),
        quantity, decimals, reissuable, fee, timestamp, _signature)
    } yield _t
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

    def toTx: Either[ValidationError, ReissueTransaction] = for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _t <- ReissueTransaction.create(_sender, _assetId, quantity, reissuable, fee, timestamp, _signature)
    } yield _t
  }

  case class AssetBurnRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                senderPublicKey: String,
                              @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                assetId: String,
                              @ApiModelProperty(required = true, example = "1000000")
                                quantity: Long,
                              @ApiModelProperty(required = true)
                                fee: Long,
                              @ApiModelProperty(required = true)
                                timestamp: Long,
                              @ApiModelProperty(required = true)
                                signature: String) {

    def toTx: Either[ValidationError, BurnTransaction] = for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t <- BurnTransaction.create(_sender, _assetId, quantity, fee, timestamp, _signature)
    } yield _t
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
                                  @ApiModelProperty(value = "Fee asset ID")
                                  feeAssetId: Option[String],
                                  @ApiModelProperty(required = true)
                                  timestamp: Long,
                                  @ApiModelProperty(value = "Base58 encoded attachment")
                                  attachment: Option[String],
                                  @ApiModelProperty(required = true)
                                  signature: String) {
    def toTx: Either[ValidationError, TransferTransaction] = for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId <- parseBase58ToOption(assetId, "invalid.assetId", AssetIdStringLength)
      _feeAssetId <- parseBase58ToOption(feeAssetId, "invalid.feeAssetId", AssetIdStringLength)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _attachment <- parseBase58(attachment, "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      _account <- if (Account.isValidAddress(recipient)) Right(new Account(recipient)) else Left(InvalidAddress)
      _t <- TransferTransaction.create(_assetId, _sender, _account, amount, timestamp, _feeAssetId, fee, _attachment,
        _signature)
    } yield _t
  }

  implicit val assetTransferRequestFormat: Format[AssetTransferRequest] = Json.format
  implicit val assetIssueRequestReads: Format[AssetIssueRequest] = Json.format
  implicit val assetReissueRequestReads: Format[AssetReissueRequest] = Json.format
  implicit val assetBurnRequestReads: Format[AssetBurnRequest] = Json.format
}
