package com.wavesplatform.transaction

import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.serialization.impl.DataTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.DataTxValidator
import monix.eval.Coeval
import play.api.libs.json._

import scala.reflect.ClassTag
import scala.util.Try

case class DataTransaction(version: TxVersion, sender: PublicKey, data: Seq[DataEntry[_]], fee: TxTimestamp, timestamp: TxTimestamp, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with TxWithFee.InWaves
    with FastHashId {

  //noinspection TypeAnnotation
  override val builder = DataTransaction

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.eval(builder.serializer.toJson(this))
}

object DataTransaction extends TransactionParserLite {
  val MaxBytes: Int      = 150 * 1024 // implicitly used for RIDE CONST_STRING and CONST_BYTESTR
  val MaxEntryCount: Int = 100

  override type TransactionT = DataTransaction

  override val typeId: TxType                    = 12
  override val supportedVersions: Set[TxVersion] = Set(1)
  override val classTag                          = ClassTag(classOf[DataTransaction])

  implicit val validator: TxValidator[DataTransaction] = DataTxValidator

  implicit def sign(tx: DataTransaction, privateKey: PrivateKey): DataTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  val serializer = DataTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[DataTransaction] =
    serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      data: Seq[DataEntry[_]],
      fee: TxAmount,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransactionT] =
    DataTransaction(version, sender, data, fee, timestamp, proofs).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      data: List[DataEntry[_]],
      fee: TxAmount,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] =
    create(version, sender, data, fee, timestamp, Proofs.empty).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      data: Seq[DataEntry[_]],
      fee: TxAmount,
      timestamp: TxTimestamp
  ): Either[ValidationError, TransactionT] =
    signed(version, sender, data, fee, timestamp, sender)
}
