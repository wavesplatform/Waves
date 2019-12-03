package com.wavesplatform.transaction.lease

import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.LeaseCancelTxSerializer
import com.wavesplatform.transaction.validation.impl.LeaseCancelTxValidator
import com.wavesplatform.transaction.{FastHashId, LegacyPBSwitch, Proofs, SigProofsSwitch, TransactionParser, TxAmount, TxTimestamp, TxType, TxVersion, TxWithFee, VersionedTransaction}
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

final case class LeaseCancelTransaction(
    version: TxVersion,
    sender: PublicKey,
    leaseId: ByteStr,
    fee: TxAmount,
    timestamp: TxTimestamp,
    proofs: Proofs
) extends SigProofsSwitch
    with VersionedTransaction
    with TxWithFee.InWaves
    with FastHashId
    with LegacyPBSwitch.V3 {
  override def builder: TransactionParser          = LeaseCancelTransaction
  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(LeaseCancelTransaction.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(LeaseCancelTransaction.serializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(LeaseCancelTransaction.serializer.toJson(this))
}

object LeaseCancelTransaction extends TransactionParser {
  type TransactionT = LeaseCancelTransaction
  val classTag: ClassTag[LeaseCancelTransaction] = ClassTag(classOf[LeaseCancelTransaction])
  val supportedVersions: Set[TxVersion]          = Set(1, 2, 3)
  val typeId: TxType                             = 9

  implicit val validator = LeaseCancelTxValidator
  val serializer         = LeaseCancelTxSerializer

  implicit def sign(tx: LeaseCancelTransaction, privateKey: PrivateKey): LeaseCancelTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[LeaseCancelTransaction] =
    serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      leaseId: ByteStr,
      fee: TxAmount,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] =
    LeaseCancelTransaction(version, sender, leaseId, fee, timestamp, proofs).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      leaseId: ByteStr,
      fee: TxAmount,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, sender, leaseId, fee, timestamp, Nil).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      leaseId: ByteStr,
      fee: TxAmount,
      timestamp: TxTimestamp
  ): Either[ValidationError, TransactionT] =
    signed(version, sender, leaseId, fee, timestamp, sender)
}
