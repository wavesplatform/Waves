package com.wavesplatform.api.http.requests

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.bls.{BlsPublicKey, BlsSignature}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Height
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, Proofs, TransactionType, TxVersion}
import play.api.libs.json.*

object CommitToGenerationRequest {
  given OFormat[CommitToGenerationRequest]       = Json.format
  given OFormat[SignedCommitToGenerationRequest] = Json.format
}

case class CommitToGenerationRequest(
    version: Option[TxVersion] = None,
    sender: Option[String],
    generationPeriodStart: Option[Height] = None,
    timestamp: Option[Long] = None,
    chainId: Option[Byte] = None
) {
  def toTxFrom(sender: PublicKey, defaultGenerationPeriodStart: Height): Either[ValidationError, CommitToGenerationTransaction] =
    for {
      tx <- CommitToGenerationTransaction.create(
        version.getOrElse(1.toByte),
        sender,
        BlsPublicKey(Array.emptyByteArray),
        generationPeriodStart.getOrElse(defaultGenerationPeriodStart),
        timestamp.getOrElse(0L),
        FeeConstants(TransactionType.CommitToGeneration) * FeeUnit,
        commitmentSignature = BlsSignature.Empty,
        Proofs.empty,
        chainId.getOrElse(AddressScheme.current.chainId)
      )
    } yield tx
}

case class SignedCommitToGenerationRequest(
    version: Option[TxVersion],
    senderPublicKey: String,
    endorserPublicKey: ByteStr,
    generationPeriodStart: Int,
    timestamp: Long,
    fee: Long,
    commitmentSignature: ByteStr,
    proofs: Proofs
) {
  def toTx: Either[ValidationError, CommitToGenerationTransaction] =
    for {
      _senderPk <- PublicKey.fromBase58String(senderPublicKey)
      sig       <- BlsSignature(commitmentSignature)
      t <- CommitToGenerationTransaction.create(
        version.getOrElse(1.toByte),
        _senderPk,
        BlsPublicKey(endorserPublicKey),
        Height(generationPeriodStart),
        timestamp,
        fee,
        sig,
        proofs,
        AddressScheme.current.chainId
      )
    } yield t
}
