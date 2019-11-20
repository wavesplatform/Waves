package com.wavesplatform.api.http.requests

import com.google.common.base.Charsets
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

object SignedIssueV1Request {
  implicit val assetIssueRequestReads: Format[SignedIssueV1Request] = Json.format
}

@ApiModel(value = "Signed Asset issue transaction")
case class SignedIssueV1Request(
    @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
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
    signature: String
) {
  def toTx: Either[ValidationError, IssueTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid signature", SignatureStringLength)
      _t <- IssueTransaction.create(
        TxVersion.V1,
        _sender,
        name.getBytes(Charsets.UTF_8),
        description.getBytes(Charsets.UTF_8),
        quantity,
        decimals,
        reissuable,
        script = None,
        fee,
        timestamp,
        Proofs(_signature)
      )
    } yield _t
}
