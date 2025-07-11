package com.wavesplatform.transaction

import com.wavesplatform.account.*
import com.wavesplatform.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
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

  implicit def signed(tx: CommitToGenerationTransaction, privateKey: PrivateKey): CommitToGenerationTransaction = {
    val blsKP      = BlsKeyPair(privateKey)
    val blsMessage = blsKP.publicKey.asByteStr.arr // TODO: What else?
    val blsSig     = blsKP.sign(blsMessage)

    val txWithBlsSig = tx.copy(
      endorsementPublicKey = blsKP.publicKey,
      endorsementKeySignature = ByteStr(blsSig)
    )

    txWithBlsSig.copy(proofs = Proofs(crypto.sign(privateKey, txWithBlsSig.bodyBytes())))
  }

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

  def selfSigned(
      sender: KeyPair,
      endorsementPublicKey: BlsPublicKey,
      endorsementKeySignature: ByteStr,
      feeInWaves: Long,
      timestamp: TxTimestamp,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, CommitToGenerationTransaction] =
    create(sender.publicKey, feeInWaves, timestamp, endorsementPublicKey, endorsementKeySignature, Proofs.empty, chainId)
      .map(signed(_, sender.privateKey))
}
