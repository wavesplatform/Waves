package scorex.api.http

import cats.implicits._
import com.wavesplatform.state.DataEntry
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json
import scorex.account.{AddressOrAlias, PublicKeyAccount}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.data.{DataTransaction, DataTransactionV1, DataTransactionV2}
import scorex.transaction.{Proofs, ValidationError}

object DataRequest {
  implicit val unsignedDataRequestReads = Json.reads[DataRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedDataRequest]
}

case class DataRequest(version: Byte, sender: String, recipient: Option[String], data: List[DataEntry[_]], fee: Long, timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
case class SignedDataRequest(@ApiModelProperty(required = true)
                             version: Byte,
                             @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                             senderPublicKey: String,
                             @ApiModelProperty()
                             recipient: Option[String],
                             @ApiModelProperty(value = "Data to put into blockchain", required = true)
                             data: List[DataEntry[_]],
                             @ApiModelProperty(required = true)
                             fee: Long,
                             @ApiModelProperty(required = true)
                             timestamp: Long,
                             @ApiModelProperty(required = true)
                             proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] = {
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _recipient <- recipient
        .traverse[Either[ValidationError, ?], AddressOrAlias](AddressOrAlias.fromString)
        .flatMap(recOpt => {
          Either.cond(
            !(version == 1 && recOpt.nonEmpty),
            recOpt,
            GenericError(s"Only transactions with version greater than 1 can have recipient")
          )
        })
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t <- version match {
        case 1 => DataTransactionV1.create(version, _sender, data, fee, timestamp, _proofs)
        case 2 => DataTransactionV2.create(version, _sender, _recipient, data, fee, timestamp, _proofs)
      }
    } yield t
  }
}
