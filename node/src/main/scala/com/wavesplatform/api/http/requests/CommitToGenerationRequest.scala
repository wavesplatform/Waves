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
  given OFormat[CommitToGenerationRequest] = Json.format
}

case class CommitToGenerationRequest(
    version: Option[TxVersion] = None,
    senderPublicKey: String,
    endorserPublicKey: ByteStr,
    generationPeriodStart: Height,
    timestamp: Option[Long] = None,
    fee: Option[Long] = None,
    commitmentSignature: ByteStr,
    chainId: Byte = AddressScheme.current.chainId,
    proofs: Proofs = Proofs.empty
) extends TxBroadcastRequest[CommitToGenerationTransaction] {
  def toTx: Either[ValidationError, CommitToGenerationTransaction] = {
    for {
      blsSignature <- BlsSignature(commitmentSignature)
      blsPk        <- BlsPublicKey(endorserPublicKey)
      senderPk     <- PublicKey.fromBase58String(senderPublicKey)
      tx <- CommitToGenerationTransaction.create(
        version.getOrElse(1.toByte),
        senderPk,
        blsPk,
        generationPeriodStart,
        timestamp.getOrElse(defaultTimestamp),
        fee.getOrElse(FeeConstants(TransactionType.CommitToGeneration) * FeeUnit),
        blsSignature,
        proofs,
        chainId
      )
    } yield tx
  }
}
