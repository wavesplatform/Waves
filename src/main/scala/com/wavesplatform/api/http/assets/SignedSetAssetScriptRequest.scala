package com.wavesplatform.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Json, OFormat}
import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs, ValidationError}

object SignedSetAssetScriptRequest {
  implicit val jsonFormatSigneSetAssetScriptRequest: OFormat[SignedSetAssetScriptRequest] = Json.format
}

@ApiModel(value = "Proven SetAssetScript transaction")
case class SignedSetAssetScriptRequest(@ApiModelProperty(required = true)
                                       version: Byte,
                                       @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                       senderPublicKey: String,
                                       @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                       assetId: String,
                                       @ApiModelProperty(value = "Base64 encoded script(including version and checksum)", required = true)
                                       script: Option[String],
                                       @ApiModelProperty(required = true)
                                       fee: Long,
                                       @ApiModelProperty(required = true)
                                       timestamp: Long,
                                       @ApiModelProperty(required = true)
                                       proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, SetAssetScriptTransaction] =
    for {
      _sender  <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _script <- script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      chainId = AddressScheme.current.chainId
      t <- SetAssetScriptTransaction.create(version, chainId, _sender, _assetId, _script, fee, timestamp, _proofs)
    } yield t
}
