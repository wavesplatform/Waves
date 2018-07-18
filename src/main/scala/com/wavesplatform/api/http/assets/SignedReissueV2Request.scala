package com.wavesplatform.api.http.assets

import cats.implicits._
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.assets.ReissueTransactionV2
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs, ValidationError}

case class SignedReissueV2Request(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                  senderPublicKey: String,
                                  @ApiModelProperty(required = true)
                                  version: Byte,
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
                                  proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, ReissueTransactionV2] =
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      chainId = AddressScheme.current.chainId
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _assetId    <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _t          <- ReissueTransactionV2.create(version, chainId, _sender, _assetId, quantity, reissuable, fee, timestamp, _proofs)
    } yield _t
}

object SignedReissueV2Request {
  implicit val assetReissueRequestReads: Format[SignedReissueV2Request] = Json.format
}
