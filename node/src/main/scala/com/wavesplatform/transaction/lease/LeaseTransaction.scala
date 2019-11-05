package com.wavesplatform.transaction.lease

import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.LeaseTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.TxFeeValidator
import com.wavesplatform.transaction.{
  FastHashId,
  Proofs,
  SigProofsSwitch,
  TransactionParserLite,
  TxAmount,
  TxTimestamp,
  TxType,
  TxVersion,
  TxWithFee,
  VersionedTransaction
}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

final case class LeaseTransaction(
    version: TxVersion,
    sender: PublicKey,
    recipient: AddressOrAlias,
    amount: TxAmount,
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs
) extends SigProofsSwitch
    with VersionedTransaction
    with TxWithFee.InWaves
    with FastHashId {
  override def builder: TransactionParserLite      = LeaseTransaction
  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(LeaseTransaction.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(LeaseTransaction.serializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(LeaseTransaction.serializer.toJson(this))
}

object LeaseTransaction extends TransactionParserLite {
  type TransactionT = LeaseTransaction
  val classTag: ClassTag[LeaseTransaction] = ClassTag(classOf[LeaseTransaction])
  val supportedVersions: Set[TxVersion]    = Set(1, 2)
  val typeId: TxType                       = 8

  implicit val validator = TxFeeValidator.asInstanceOf[TxValidator[LeaseTransaction]]
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
    LeaseTransaction(version, sender, recipient, amount, fee, timestamp, proofs).validatedEither

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
    signed(version, sender, recipient, amount, fee, timestamp, sender)
}
