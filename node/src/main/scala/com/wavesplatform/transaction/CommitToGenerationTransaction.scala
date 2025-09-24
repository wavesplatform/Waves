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
    endorserPublicKey: BlsPublicKey,
    generationPeriodStart: Height,
    timestamp: TxTimestamp,
    fee: TxPositiveAmount,
    commitmentSignature: BlsSignature,
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
        "endorserPublicKey"     -> endorserPublicKey.base64,
        "generationPeriodStart" -> generationPeriodStart,
        "commitmentSignature"   -> commitmentSignature.base64
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

    val txWithBlsSig = tx.copy(endorserPublicKey = blsKP.publicKey, commitmentSignature = blsSig)
    txWithBlsSig.copy(proofs = Proofs(crypto.sign(privateKey, txWithBlsSig.bodyBytes())))
  }

  def create(
      sender: PublicKey,
      endorserPublicKey: BlsPublicKey,
      generationPeriodStart: Height,
      timestamp: TxTimestamp,
      feeInWaves: Long,
      commitmentSignature: BlsSignature,
      proofs: Proofs,
      chainId: Byte
  ): Either[ValidationError, CommitToGenerationTransaction] =
    for {
      feeInWaves <- TxPositiveAmount(feeInWaves)(TxValidationError.InsufficientFee)
      tx <- CommitToGenerationTransaction(
        sender,
        endorserPublicKey,
        generationPeriodStart,
        timestamp,
        feeInWaves,
        commitmentSignature,
        proofs,
        chainId
      ).validatedEither
    } yield tx

  def selfSigned(
      sender: KeyPair,
      endorserPublicKey: BlsPublicKey,
      generationPeriodStart: Height,
      timestamp: TxTimestamp,
      feeInWaves: Long,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, CommitToGenerationTransaction] =
    create(
      sender.publicKey,
      endorserPublicKey,
      generationPeriodStart,
      timestamp,
      feeInWaves,
      commitmentSignature = BlsSignature.Empty,
      Proofs.empty,
      chainId
    ).map(signed(_, sender.privateKey))
}
