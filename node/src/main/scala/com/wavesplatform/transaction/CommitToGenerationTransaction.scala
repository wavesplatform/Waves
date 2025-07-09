package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.finalization.BlsPublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.CommitToGenerationTxValidator
import monix.eval.Coeval
import play.api.libs.json.*

final case class CommitToGenerationTransaction(
    sender: PublicKey,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    endorsementPublicKey: BlsPublicKey,
    endorsementKeySignature: ByteStr,
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
        "endorsementPublicKey"    -> endorsementPublicKey.asByteStr.toString,
        "endorsementKeySignature" -> endorsementKeySignature.toString
      )
    )
}

object CommitToGenerationTransaction {
  implicit val validator: TxValidator[CommitToGenerationTransaction] = CommitToGenerationTxValidator

  implicit def sign(tx: CommitToGenerationTransaction, privateKey: PrivateKey): CommitToGenerationTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  def create(
      sender: PublicKey,
      feeInWaves: Long,
      timestamp: TxTimestamp,
      endorsementPublicKey: BlsPublicKey,
      endorsementKeySignature: ByteStr,
      proofs: Proofs,
      chainId: Byte
  ): Either[ValidationError, CommitToGenerationTransaction] =
    for {
      feeInWaves <- TxPositiveAmount(feeInWaves)(TxValidationError.InsufficientFee)
      tx <- CommitToGenerationTransaction(
        sender,
        feeInWaves,
        timestamp,
        endorsementPublicKey,
        endorsementKeySignature,
        proofs,
        chainId
      ).validatedEither
    } yield tx
}
