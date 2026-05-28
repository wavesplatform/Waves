package com.wavesplatform.transaction.lease

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.serialization.impl.LeaseCancelTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.LeaseCancelTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

final case class LeaseCancelTransaction(
    version: TxVersion,
    sender: PublicKey,
    leaseId: ByteStr,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.LeaseCancel),
      ProvenTransaction,
      HasSignature,
      Versioned.ToV3,
      TxWithFee.InWaves,
      FastHashId,
      PBSince.V3 {
  override type T = LeaseCancelTransaction

  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(LeaseCancelTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(LeaseCancelTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(LeaseCancelTxSerializer.toJson(this))

  override def addProof(proof: ByteStr): LeaseCancelTransaction = copy(proofs = this.proofs.add(proof))
}

object LeaseCancelTransaction extends TransactionParser {
  type TransactionT = LeaseCancelTransaction

  val typeId: TxType = 9: Byte

  implicit val validator: TxValidator[LeaseCancelTransaction] = LeaseCancelTxValidator

  override def parseBytes(bytes: Array[Byte]): Try[LeaseCancelTransaction] =
    LeaseCancelTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      leaseId: ByteStr,
      fee: Long,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransactionT] =
    for {
      fee <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx  <- LeaseCancelTransaction(version, sender, leaseId, fee, timestamp, proofs, chainId).validatedEither
    } yield tx
}
