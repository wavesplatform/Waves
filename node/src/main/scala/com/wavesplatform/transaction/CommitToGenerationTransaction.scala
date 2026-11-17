package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
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
    override val version: TxVersion,
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

  override type T = CommitToGenerationTransaction

  override def addProof(proof: ByteStr): CommitToGenerationTransaction = copy(proofs = proofs.add(proof))

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBTransactionSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(PBTransactionSerializer.bytes(this))
  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      BaseTxJson.toJson(this) ++ Json.obj(
        "endorserPublicKey"     -> endorserPublicKey.base58,
        "generationPeriodStart" -> generationPeriodStart,
        "commitmentSignature"   -> commitmentSignature.base58
      )
    )

  /** @param senderBound
    *   whether [[com.wavesplatform.features.BlockchainFeatures.FinalityImprovements]] is activated, see [[CommitToGenerationTransaction.mkPopMessage]]
    */
  def popMessage(senderBound: Boolean): Array[Byte] =
    CommitToGenerationTransaction.mkPopMessage(endorserPublicKey, generationPeriodStart, Option.when(senderBound)(sender))
}

object CommitToGenerationTransaction {
  val DepositInWavelets = 100_00000000L

  implicit val validator: TxValidator[CommitToGenerationTransaction] = CommitToGenerationTxValidator

  implicit def signed(tx: CommitToGenerationTransaction, privateKey: PrivateKey): CommitToGenerationTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  /** @param sender See [[mkPopMessage]] */
  def mkPopSignature(blsKeyPair: BlsKeyPair, generationPeriodStart: Height, sender: Option[PublicKey]): BlsSignature =
    blsKeyPair.sign(mkPopMessage(blsKeyPair.publicKey, generationPeriodStart, sender))

  /** @param sender
    *   the transaction's sender since [[com.wavesplatform.features.BlockchainFeatures.FinalityImprovements]], None before it. A proof that does
    *   not name its sender can be copied from a pending transaction into another account's commitment, which takes the BLS key for the period
    *   and locks its owner out.
    */
  def mkPopMessage(blsPublicKey: BlsPublicKey, generationPeriodStart: Height, sender: Option[PublicKey]): Array[Byte] =
    blsPublicKey.arr ++ generationPeriodStart.toByteArray ++ sender.fold(Array.emptyByteArray)(_.arr)

  def create(
      version: TxVersion,
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
        version,
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
      version: TxVersion,
      sender: KeyPair,
      endorserPublicKey: BlsPublicKey,
      generationPeriodStart: Height,
      timestamp: TxTimestamp,
      feeInWaves: Long,
      commitmentSignature: BlsSignature,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, CommitToGenerationTransaction] =
    create(version, sender.publicKey, endorserPublicKey, generationPeriodStart, timestamp, feeInWaves, commitmentSignature, Proofs.empty, chainId)
      .map(signed(_, sender.privateKey))
}
