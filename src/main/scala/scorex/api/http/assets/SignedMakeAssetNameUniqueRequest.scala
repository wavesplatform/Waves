package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.TransactionParser.SignatureStringLength
import scorex.transaction.{AssetIdStringLength, ValidationError}
import scorex.transaction.assets.MakeAssetNameUniqueTransaction


case class SignedMakeAssetNameUniqueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                            senderPublicKey: String,
                                            @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                            assetId: String,
                                            @ApiModelProperty(required = true, example = "1000000")
                                            fee: Long,
                                            @ApiModelProperty(required = true)
                                            networkByte: Byte,
                                            @ApiModelProperty(required = true)
                                            timestamp: Long,
                                            @ApiModelProperty(required = true)
                                            signature: String
                                           ) extends BroadcastRequest {
  def toTx: Either[ValidationError, MakeAssetNameUniqueTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _t <- MakeAssetNameUniqueTransaction.create(_sender, _assetId.arr, fee, timestamp, networkByte, _signature)
  } yield _t
}


object SignedMakeAssetNameUniqueRequest {
  implicit val signedMakeAssetUniqueFormat: Format[SignedMakeAssetNameUniqueRequest] = Json.format
}
