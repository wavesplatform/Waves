package com.wavesplatform.transaction.lease

import com.wavesplatform.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
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
) extends Transaction(TransactionType.LeaseCancel)
    with SigProofsSwitch
    with VersionedTransaction.ToV3
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V3 {
  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(LeaseCancelTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(LeaseCancelTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(LeaseCancelTxSerializer.toJson(this))
}

object LeaseCancelTransaction extends TransactionParser {
  type TransactionT = LeaseCancelTransaction

  val typeId: TxType = 9: Byte

  implicit val validator: TxValidator[LeaseCancelTransaction] = LeaseCancelTxValidator

  implicit def sign(tx: LeaseCancelTransaction, privateKey: PrivateKey): LeaseCancelTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

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

  def signed(
      version: TxVersion,
      sender: PublicKey,
      leaseId: ByteStr,
      fee: Long,
      timestamp: TxTimestamp,
      signer: PrivateKey,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransactionT] =
    create(version, sender, leaseId, fee, timestamp, Nil, chainId).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      leaseId: ByteStr,
      fee: Long,
      timestamp: TxTimestamp,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransactionT] =
    signed(version, sender.publicKey, leaseId, fee, timestamp, sender.privateKey, chainId).map(_.signWith(sender.privateKey))
}
