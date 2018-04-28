package scorex.api.http.leasing

import cats.implicits._
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.TransactionParsers.SignatureStringLength
import scorex.transaction.lease.LeaseCancelTransactionV2
import scorex.transaction.{Proofs, ValidationError}

case class SignedLeaseCancelV2Request(@ApiModelProperty(required = true)
                                      version: Byte,
                                      @ApiModelProperty(required = true)
                                      chainId: Byte,
                                      @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                      senderPublicKey: String,
                                      @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                      txId: String,
                                      @ApiModelProperty(required = true)
                                      timestamp: Long,
                                      @ApiModelProperty(required = true)
                                      proofs: List[String],
                                      @ApiModelProperty(required = true)
                                      fee: Long)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseCancelTransactionV2] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _leaseTx    <- parseBase58(txId, "invalid.leaseTx", SignatureStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- LeaseCancelTransactionV2.create(version, chainId, _sender, _leaseTx, fee, timestamp, _proofs)
    } yield _t
}
object SignedLeaseCancelV2Request {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseCancelV2Request] = Json.format
}
