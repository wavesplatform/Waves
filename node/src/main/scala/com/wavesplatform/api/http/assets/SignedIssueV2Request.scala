package com.wavesplatform.api.http.assets

import cats.implicits._
import com.google.common.base.Charsets
import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV2}
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.{Proofs, ValidationError}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._

object SignedIssueV2Request {
  implicit val signedExchangeRequestReads: Reads[SignedIssueV2Request] = {
    (
      (JsPath \ "senderPublicKey").read[String] and
        (JsPath \ "name").read[String] and
        (JsPath \ "description").read[String] and
        (JsPath \ "quantity").read[Long] and
        (JsPath \ "decimals").read[Byte] and
        (JsPath \ "reissuable").read[Boolean] and
        (JsPath \ "fee").read[Long] and
        (JsPath \ "timestamp").read[Long] and
        (JsPath \ "proofs").read[List[ProofStr]] and
        (JsPath \ "script").readNullable[String]
    )(SignedIssueV2Request.apply _)
  }
  implicit val writes: Writes[SignedIssueV2Request] =
    Json
      .writes[SignedIssueV2Request]
      .transform(
        (request: JsObject) =>
          request + ("version" -> JsNumber(2))
            + ("type"          -> JsNumber(IssueTransaction.typeId.toInt)))
}

@ApiModel(value = "Signed Smart issue transaction")
case class SignedIssueV2Request(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
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
                                proofs: List[String],
                                @ApiModelProperty(value = "Base58 encoded compiled asset script")
                                script: Option[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, IssueTransactionV2] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _script <- script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase64String(s).map(Some(_))
      }
      t <- IssueTransactionV2.create(
        AddressScheme.current.chainId,
        _sender,
        name.getBytes(Charsets.UTF_8),
        description.getBytes(Charsets.UTF_8),
        quantity,
        decimals,
        reissuable,
        _script,
        fee,
        timestamp,
        _proofs
      )
    } yield t
}
