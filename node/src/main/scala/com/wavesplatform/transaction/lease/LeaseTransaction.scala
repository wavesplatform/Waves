package com.wavesplatform.transaction.lease

import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.LeaseTxSerializer
import com.wavesplatform.transaction.validation.impl.LeaseTxValidator
import com.wavesplatform.transaction._
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

final case class LeaseTransaction(
    version: TxVersion,
    sender: PublicKey,
    recipient: AddressOrAlias,
    amount: TxAmount,
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends SigProofsSwitch
    with VersionedTransaction
    with TxWithFee.InWaves
    with FastHashId
    with LegacyPBSwitch.V3 {
  override def builder: TransactionParser          = LeaseTransaction
  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(LeaseTransaction.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(LeaseTransaction.serializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(LeaseTransaction.serializer.toJson(this))
}

object LeaseTransaction extends TransactionParser {
  type TransactionT = LeaseTransaction

  val supportedVersions: Set[TxVersion] = Set(1, 2, 3)
  val typeId: TxType                    = 8: Byte

  implicit val validator = LeaseTxValidator
  val serializer         = LeaseTxSerializer

  implicit def sign(tx: LeaseTransaction, privateKey: PrivateKey): LeaseTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[LeaseTransaction] =
    serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: TxAmount,
      fee: TxAmount,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] =
    LeaseTransaction(version, sender, recipient, amount, fee, timestamp, proofs, recipient.chainId).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: TxAmount,
      fee: TxAmount,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, sender, recipient, amount, fee, timestamp, Nil).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      recipient: AddressOrAlias,
      amount: TxAmount,
      fee: TxAmount,
      timestamp: TxTimestamp
  ): Either[ValidationError, TransactionT] =
    signed(version, sender.publicKey, recipient, amount, fee, timestamp, sender.privateKey)
}
