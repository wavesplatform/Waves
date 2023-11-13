package com.wavesplatform.transaction.lease

import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.serialization.impl.LeaseTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.LeaseTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

final case class LeaseTransaction(
    version: TxVersion,
    sender: PublicKey,
    recipient: AddressOrAlias,
    amount: TxPositiveAmount,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.Lease)
    with SigProofsSwitch
    with VersionedTransaction.ToV3
    with TxWithFee.InWaves
    with FastHashId
    with PBSince.V3 {
  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(LeaseTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(LeaseTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(LeaseTxSerializer.toJson(this))
}

object LeaseTransaction extends TransactionParser {
  type TransactionT = LeaseTransaction

  val typeId: TxType = 8: Byte

  implicit val validator: TxValidator[LeaseTransaction] = LeaseTxValidator

  implicit def sign(tx: LeaseTransaction, privateKey: PrivateKey): LeaseTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[LeaseTransaction] =
    LeaseTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: Long,
      fee: Long,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] = {
    for {
      fee    <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      amount <- TxPositiveAmount(amount)(TxValidationError.NonPositiveAmount(amount, "waves"))
      tx     <- LeaseTransaction(version, sender, recipient, amount, fee, timestamp, proofs, recipient.chainId).validatedEither
    } yield tx

  }

  def signed(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: Long,
      fee: Long,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, sender, recipient, amount, fee, timestamp, Nil).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      recipient: AddressOrAlias,
      amount: Long,
      fee: Long,
      timestamp: TxTimestamp
  ): Either[ValidationError, TransactionT] =
    signed(version, sender.publicKey, recipient, amount, fee, timestamp, sender.privateKey)
}
