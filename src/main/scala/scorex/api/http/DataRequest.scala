package scorex.api.http

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.transaction.{DataTransaction, Proofs, ValidationError}
import scorex.transaction.DataTransaction.Item

object DataRequest {
  implicit val unsignedFormat: Format[DataRequest] = Json.format
  implicit val signedFormat: Format[SignedDataRequest] = Json.format
}

case class DataRequest(sender: String,
                       data: List[Item],
                       fee: Long,
                       timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
case class SignedDataRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                             senderPublicKey: String,
                             @ApiModelProperty(value = "///", required = true)
                             data: List[Item],
                             @ApiModelProperty(required = true)
                             fee: Long,
                             @ApiModelProperty(required = true)
                             timestamp: Long,
                             @ApiModelProperty(required = true)
                             proofs: List[String]) extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _data <- data.traverse(_.parse)
    _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
    _proofs <- Proofs.create(_proofBytes)
    t <- DataTransaction.create(Proofs.Version, _sender, _data, timestamp, fee, _proofs) ///data
  } yield t
}
