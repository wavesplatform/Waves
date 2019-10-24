package com.wavesplatform.transaction.transfer

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.TxSerializer
import com.wavesplatform.transaction.serialization.impl.TransferTxSerializer
import com.wavesplatform.transaction.sign.TxSigner
import com.wavesplatform.transaction.validation._
import com.wavesplatform.transaction.validation.impl.TransferTxValidator
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.reflect.ClassTag
import scala.util.Try

case class TransferTransaction(
    version: Byte,
    timestamp: Long,
    sender: PublicKey,
    recipient: AddressOrAlias,
    assetId: Asset,
    amount: Long,
    feeAssetId: Asset,
    fee: Long,
    attachment: Array[Byte],
    proofs: Proofs
) extends VersionedTransaction
    with SignatureField
    with FastHashId {

  override val typeId: Byte = TransferTransaction.typeId

  override val assetFee: (Asset, Long) = (feeAssetId, fee)

  // TODO: Rework caching
  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(TxSerializer[TransferTransaction].bodyBytes(this))
  val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(TxSerializer[TransferTransaction].toBytes(this))
  final val json: Coeval[JsObject]   = Coeval.evalOnce(TxSerializer[TransferTransaction].toJson(this))

  override def checkedAssets(): Seq[IssuedAsset] = assetId match {
    case a: IssuedAsset => Seq(a)
    case Waves          => Nil
  }

  override def builder: TransactionParserLite = TransferTransaction
}

object TransferTransaction extends TransactionParserLite {
  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  implicit val serializer: TxSerializer[TransferTransaction] = TransferTxSerializer
  implicit val validator: TxValidator[TransferTransaction]   = TransferTxValidator
  implicit val signer: TxSigner[TransferTransaction] = (tx: TransferTransaction, privateKey: PrivateKey) =>
    tx.copy(proofs = Proofs(Seq(ByteStr(crypto.sign(privateKey, tx.bodyBytes())))))

  val typeId: Byte = 4.toByte

  override def supportedVersions: Set[Byte] = Set(1.toByte, 2.toByte)

  override type TransactionT = TransferTransaction

  override def classTag: ClassTag[TransferTransaction] = ClassTag(classOf[TransferTransaction])

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = serializer.parseBytes(bytes)

  // create
  def apply(
      version: Byte,
      asset: Asset,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: Long,
      timestamp: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: Array[Byte],
      proofs: Proofs
  ): Either[ValidationError, TransferTransaction] =
    TransferTransaction(version, timestamp, sender, recipient, asset, amount, feeAsset, fee, attachment, proofs).validatedEither

  // signed
  def apply(
      version: Byte,
      asset: Asset,
      sender: PublicKey,
      recipient: AddressOrAlias,
      amount: Long,
      timestamp: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: Array[Byte],
      signer: PrivateKey
  ): Either[ValidationError, TransferTransaction] =
    apply(version, asset, sender, recipient, amount, timestamp, feeAsset, fee, attachment, Proofs.empty)
      .map(_.signWith(signer))

  // selfSigned
  def selfSigned(
      version: Byte,
      asset: Asset,
      sender: KeyPair,
      recipient: AddressOrAlias,
      amount: Long,
      timestamp: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: Array[Byte]
  ): Either[ValidationError, TransferTransaction] =
    apply(version, asset, sender, recipient, amount, timestamp, feeAsset, fee, attachment, sender)
}
