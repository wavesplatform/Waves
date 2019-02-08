package com.wavesplatform.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.assets.IssueTransactionV1

object SignedIssueV1Request {
  implicit val assetIssueRequestReads: Format[SignedIssueV1Request] = Json.format
}

@ApiModel(value = "Signed Asset issue transaction")
case class SignedIssueV1Request(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
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
                                signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, IssueTransactionV1] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid signature", SignatureStringLength)
      _t <- IssueTransactionV1.create(_sender,
                                      name.getBytes(Charsets.UTF_8),
                                      description.getBytes(Charsets.UTF_8),
                                      quantity,
                                      decimals,
                                      reissuable,
                                      fee,
                                      timestamp,
                                      _signature)
    } yield _t
}
