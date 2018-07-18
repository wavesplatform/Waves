package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.state.DataEntry
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.transaction.{DataTransaction, Proofs, ValidationError}

object DataRequest {
  implicit val unsignedDataRequestReads = Json.reads[DataRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedDataRequest]
}

case class DataRequest(version: Byte, sender: String, data: List[DataEntry[_]], fee: Long, timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
case class SignedDataRequest(@ApiModelProperty(required = true)
                             version: Byte,
                             @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                             senderPublicKey: String,
                             @ApiModelProperty(value = "Data to put into blockchain", required = true)
                             data: List[DataEntry[_]],
                             @ApiModelProperty(required = true)
                             fee: Long,
                             @ApiModelProperty(required = true)
                             timestamp: Long,
                             @ApiModelProperty(required = true)
                             proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- DataTransaction.create(version, _sender, data, fee, timestamp, _proofs)
    } yield t
}
