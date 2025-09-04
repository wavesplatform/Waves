package com.wavesplatform.transaction

import com.google.common.primitives.Ints
import com.wavesplatform.account.*
import com.wavesplatform.crypto
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey, BlsSignature}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.serialization.impl.{BaseTxJson, PBTransactionSerializer}
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.CommitToGenerationTxValidator
import monix.eval.Coeval
import play.api.libs.json.*

final case class CommitToGenerationTransaction(
    sender: PublicKey,
    endorsementPublicKey: BlsPublicKey,
    generationPeriodStart: Height,
    timestamp: TxTimestamp,
    fee: TxPositiveAmount,
    endorsementKeySignature: BlsSignature,
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
        "endorsementPublicKey"    -> endorsementPublicKey.base64,
        "endorsementKeySignature" -> endorsementKeySignature.base64,
        "generationPeriodStart"   -> generationPeriodStart
      )
    )
}

object CommitToGenerationTransaction {
  val DepositInWavelets = 100_00000000L

  implicit val validator: TxValidator[CommitToGenerationTransaction] = CommitToGenerationTxValidator

  implicit def signed(tx: CommitToGenerationTransaction, privateKey: PrivateKey): CommitToGenerationTransaction = {
    val blsKP      = BlsKeyPair(privateKey)
    val blsMessage = blsKP.publicKey.arr ++ Ints.toByteArray(tx.generationPeriodStart)
    val blsSig     = blsKP.sign(blsMessage)

    val txWithBlsSig = tx.copy(endorsementPublicKey = blsKP.publicKey, endorsementKeySignature = blsSig)
    txWithBlsSig.copy(proofs = Proofs(crypto.sign(privateKey, txWithBlsSig.bodyBytes())))
  }

  def create(
      sender: PublicKey,
      endorsementPublicKey: BlsPublicKey,
      generationPeriodStart: Height,
      timestamp: TxTimestamp,
      feeInWaves: Long,
      endorsementKeySignature: BlsSignature,
      proofs: Proofs,
      chainId: Byte
  ): Either[ValidationError, CommitToGenerationTransaction] =
    for {
      feeInWaves <- TxPositiveAmount(feeInWaves)(TxValidationError.InsufficientFee)
      tx <- CommitToGenerationTransaction(
        sender,
        endorsementPublicKey,
        generationPeriodStart,
        timestamp,
        feeInWaves,
        endorsementKeySignature,
        proofs,
        chainId
      ).validatedEither
    } yield tx

  def selfSigned(
      sender: KeyPair,
      endorsementPublicKey: BlsPublicKey,
      generationPeriodStart: Height,
      timestamp: TxTimestamp,
      feeInWaves: Long,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, CommitToGenerationTransaction] =
    create(
      sender.publicKey,
      endorsementPublicKey,
      generationPeriodStart,
      timestamp,
      feeInWaves,
      endorsementKeySignature = BlsSignature.empty,
      Proofs.empty,
      chainId
    ).map(signed(_, sender.privateKey))
}
