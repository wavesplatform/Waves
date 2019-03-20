package com.wavesplatform.api.http.alias

import cats.implicits._
import com.wavesplatform.account.{Alias, PublicKeyAccount}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.transaction.{CreateAliasTransaction, CreateAliasTransactionV2, Proofs, ValidationError}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class SignedCreateAliasV2Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                      senderPublicKey: String,
                                      @ApiModelProperty(required = true)
                                      fee: Long,
                                      @ApiModelProperty(value = "Alias", required = true)
                                      alias: String,
                                      @ApiModelProperty(required = true)
                                      timestamp: Long,
                                      @ApiModelProperty(required = true)
                                      proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateAliasTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _alias      <- Alias.create(alias)
      _t          <- CreateAliasTransactionV2.create(_sender, _alias, fee, timestamp, _proofs)
    } yield _t
}

object SignedCreateAliasV2Request {
  implicit val broadcastAliasV2RequestReadsFormat: Format[SignedCreateAliasV2Request] = Json.format
}
