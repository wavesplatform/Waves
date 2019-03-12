package com.wavesplatform.api.http.assets

import cats.implicits._
import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}

object SignedSetAssetScriptRequest {

  implicit val signedSetAssetScriptRequestReads: Reads[SignedSetAssetScriptRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "script").readNullable[String] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedSetAssetScriptRequest.apply _)
}

@ApiModel(value = "Proven SetAssetScript transaction")
case class SignedSetAssetScriptRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
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
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _asset  <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength).map(IssuedAsset)
      _script <- script match {
        case None | Some("") => Right(None)
        case Some(s)         => Script.fromBase64String(s).map(Some(_))
      }
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      chainId = AddressScheme.current.chainId
      t <- SetAssetScriptTransaction.create(chainId, _sender, _asset, _script, fee, timestamp, _proofs)
    } yield t
}
