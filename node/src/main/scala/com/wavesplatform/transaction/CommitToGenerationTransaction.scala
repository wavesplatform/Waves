package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.finalization.BlsPublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.CommitToGenerationTxValidator
import monix.eval.Coeval
import play.api.libs.json.*

final case class CommitToGenerationTransaction(
    sender: PublicKey,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    generationPeriodStart: Height,
    endorsementPublicKey: BlsPublicKey,
    proofs: Proofs,
    override val chainId: Byte
) extends Transaction(TransactionType.CommitToGeneration)
    with ProvenTransaction
    with Versioned.ConstV1
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V1 {
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBTransactionSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(PBTransactionSerializer.bytes(this))
  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      BaseTxJson.toJson(this) ++ Json.obj(
        "generationPeriodStart" -> generationPeriodStart,
        "endorsementPublicKey"  -> endorsementPublicKey.asByteStr.toString
      )
    )
}

object CommitToGenerationTransaction {
  implicit val validator: TxValidator[CommitToGenerationTransaction] = CommitToGenerationTxValidator

  def create(
      sender: PublicKey,
      feeInWaves: Long,
      timestamp: TxTimestamp,
      generationPeriodStart: Height,
      endorsementPublicKey: BlsPublicKey,
      proofs: Proofs,
      chainId: Byte
  ): Either[ValidationError, CommitToGenerationTransaction] =
    for {
      feeInWaves <- TxPositiveAmount(feeInWaves)(TxValidationError.InsufficientFee)
      tx <- CommitToGenerationTransaction(sender, feeInWaves, timestamp, generationPeriodStart, endorsementPublicKey, proofs, chainId).validatedEither
    } yield tx
}
