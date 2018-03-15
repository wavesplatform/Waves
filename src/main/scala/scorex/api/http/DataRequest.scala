package scorex.api.http

import cats.implicits._
import com.wavesplatform.state2.DataEntry
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.transaction.{DataTransaction, Proofs, ValidationError}

object DataRequest {
  implicit val unsignedFormat: Format[DataRequest] = Json.format
  implicit val signedFormat: Format[SignedDataRequest] = Json.format
}

case class DataRequest(version: Byte,
                       sender: String,
                       data: List[DataEntry[_]],
                       fee: Long,
                       timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
case class SignedDataRequest(@ApiModelProperty(required = true)
                             version: Byte,
                             @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                             senderPublicKey: String,
                             @ApiModelProperty(value = "///", required = true)
                             data: List[DataEntry[_]],
                             @ApiModelProperty(required = true)
                             fee: Long,
                             @ApiModelProperty(required = true)
                             timestamp: Long,
                             @ApiModelProperty(required = true)
                             proofs: List[String]) extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
    _proofs <- Proofs.create(_proofBytes)
    t <- DataTransaction.create(version, _sender, data, fee, timestamp, _proofs)
  } yield t
}
