package com.wavesplatform.api.http.requests

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Height
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, Proofs, TransactionType}
import play.api.libs.json.*

object CommitToGenerationRequest {
  given Reads[CommitToGenerationRequest]       = Json.reads
  given Reads[SignedCommitToGenerationRequest] = Json.reads
}

case class CommitToGenerationRequest(
    sender: Option[String],
    generationPeriodStart: Option[Int],
    timestamp: Option[Long],
    chainId: Option[Byte]
) {
  def toTxFrom(sender: PublicKey, defaultGenerationPeriodStart: Height): Either[ValidationError, CommitToGenerationTransaction] =
    for {
      tx <- CommitToGenerationTransaction.create(
        sender,
        BlsPublicKey(Array.emptyByteArray),
        Height(generationPeriodStart.getOrElse(defaultGenerationPeriodStart)),
        timestamp.getOrElse(0L),
        FeeConstants(TransactionType.CommitToGeneration) * FeeUnit,
        endorsementKeySignature = BlsSignature.Empty,
        Proofs.empty,
        chainId.getOrElse(AddressScheme.current.chainId)
      )
    } yield tx
}

case class SignedCommitToGenerationRequest(
    senderPublicKey: String,
    endorsementPublicKey: ByteStr,
    generationPeriodStart: Int,
    timestamp: Long,
    fee: Long,
    endorsementKeySignature: ByteStr,
    proofs: Proofs
) {
  def toTx: Either[ValidationError, CommitToGenerationTransaction] =
    for {
      _sender <- PublicKey.fromBase58String(senderPublicKey)
      sig     <- BlsSignature(endorsementKeySignature)
      t <- CommitToGenerationTransaction.create(
        _sender,
        BlsPublicKey(endorsementPublicKey),
        Height(generationPeriodStart),
        timestamp,
        fee,
        sig,
        proofs,
        AddressScheme.current.chainId
      )
    } yield t
}
