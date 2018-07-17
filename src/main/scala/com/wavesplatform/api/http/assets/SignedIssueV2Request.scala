package com.wavesplatform.api.http.assets

import cats.implicits._
import com.google.common.base.Charsets
import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}

object SignedIssueV2Request {
  implicit val signedExchangeRequestFormat: Format[SignedIssueV2Request] = Json.format
}

@ApiModel(value = "Signed Smart issue transaction")
case class SignedIssueV2Request(@ApiModelProperty(required = true)
                                version: Byte,
                                @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                senderPublicKey: String,
                                @ApiModelProperty(required = true)
                                name: String,
                                @ApiModelProperty(required = true)
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
                                proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, IssueTransactionV2] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t <- IssueTransactionV2.create(
        version,
        AddressScheme.current.chainId,
        _sender,
        name.getBytes(Charsets.UTF_8),
        description.getBytes(Charsets.UTF_8),
        quantity,
        decimals,
        reissuable,
        None,
        fee,
        timestamp,
        _proofs
      )
    } yield t
}
