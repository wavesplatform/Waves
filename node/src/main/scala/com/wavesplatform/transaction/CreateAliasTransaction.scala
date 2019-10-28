package com.wavesplatform.transaction

import com.wavesplatform.account.{Alias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.TxSerializer
import com.wavesplatform.transaction.serialization.impl.CreateAliasTxSerializer
import com.wavesplatform.transaction.sign.TxSigner
import com.wavesplatform.transaction.validation.TxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

final case class CreateAliasTransaction(version: TxVersion, timestamp: TxTimestamp, sender: PublicKey, alias: Alias, fee: TxAmount, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with TxWithFee.InWaves
    with FastHashId {
  override def builder: TransactionParserLite      = CreateAliasTransaction
  override val bodyBytes: Coeval[Array[TxVersion]] = Coeval.evalOnce(CreateAliasTransaction.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[TxVersion]]     = Coeval.evalOnce(CreateAliasTransaction.serializer.toBytes(this))
  override val json: Coeval[JsObject]              = Coeval.evalOnce(CreateAliasTransaction.serializer.toJson(this))
}

object CreateAliasTransaction extends TransactionParserLite with TransactionOps {
  type TransactionT = CreateAliasTransaction
  val classTag: ClassTag[CreateAliasTransaction] = ClassTag(classOf[CreateAliasTransaction])
  val supportedVersions: Set[TxVersion]          = Set(1, 2)
  val typeId: Byte                               = 10

  implicit val signer: TxSigner[CreateAliasTransaction]         = (tx, privateKey) => tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))
  implicit val serializer: TxSerializer[CreateAliasTransaction] = CreateAliasTxSerializer
  implicit val validator: TxValidator[CreateAliasTransaction]   = ???

  override def parseBytes(bytes: Array[TxVersion]): Try[CreateAliasTransaction] = serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      timestamp: TxTimestamp,
      sender: PublicKey,
      alias: Alias,
      fee: TxAmount,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] =
    CreateAliasTransaction(version, timestamp, sender, alias, fee, proofs).validatedEither

  def signed(
      version: TxVersion,
      timestamp: TxTimestamp,
      sender: PublicKey,
      alias: Alias,
      fee: TxAmount,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, timestamp, sender, alias, fee, Nil).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      timestamp: TxTimestamp,
      sender: KeyPair,
      alias: Alias,
      fee: TxAmount
  ): Either[ValidationError, TransactionT] =
    signed(version, timestamp, sender, alias, fee, sender)
}
