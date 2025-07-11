package com.wavesplatform.api.http.requests

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.bls.BlsPublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{CommitToGenerationTransaction, Proofs}
import play.api.libs.json.*

object CommitToGenerationRequest {
  given Reads[CommitToGenerationRequest]       = Json.reads
  given Reads[SignedCommitToGenerationRequest] = Json.reads
}

case class CommitToGenerationRequest(
    chainId: Option[Byte],
    sender: Option[String],
    fee: Long,
    timestamp: Option[Long],
    endorsementPublicKey: ByteStr,
    endorsementKeySignature: ByteStr
) {
  def toTxFrom(sender: PublicKey): Either[ValidationError, CommitToGenerationTransaction] =
    for {
      tx <- CommitToGenerationTransaction.create(
        sender,
        fee,
        timestamp.getOrElse(0L),
        BlsPublicKey(endorsementPublicKey),
        endorsementKeySignature,
        Proofs.empty,
        chainId.getOrElse(AddressScheme.current.chainId)
      )
    } yield tx
}

case class SignedCommitToGenerationRequest(
    sender: String,
    fee: Long,
    timestamp: Long,
    endorsementPublicKey: ByteStr,
    endorsementKeySignature: ByteStr,
    proofs: Proofs
) {
  def toTx: Either[ValidationError, CommitToGenerationTransaction] =
    for {
      _sender <- PublicKey.fromBase58String(sender)
      t <- CommitToGenerationTransaction.create(
        _sender,
        fee,
        timestamp,
        BlsPublicKey(endorsementPublicKey),
        endorsementKeySignature,
        proofs,
        AddressScheme.current.chainId
      )
    } yield t
}
