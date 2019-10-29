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
    version: TxVersion,
    sender: PublicKey,
    recipient: AddressOrAlias,
    assetId: Asset,
    amount: TxAmount,
    feeAssetId: Asset,
    fee: TxAmount,
    attachment: TxByteArray,
    timestamp: TxTimestamp,
    proofs: Proofs
) extends VersionedTransaction
    with SigProofsSwitch
    with FastHashId
    with TxWithFee.InCustomAsset {

  override val typeId: Byte = TransferTransaction.typeId

  // TODO: Rework caching
  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(TransferTransaction.serializer.bodyBytes(this))
  val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(TransferTransaction.serializer.toBytes(this))
  final val json: Coeval[JsObject]   = Coeval.evalOnce(TransferTransaction.serializer.toJson(this))

  override def checkedAssets(): Seq[IssuedAsset] = assetId match {
    case a: IssuedAsset => Seq(a)
    case Waves          => Nil
  }

  override def builder: TransactionParserLite = TransferTransaction
}

object TransferTransaction extends TransactionParserLite with TransactionOps {
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

  def create(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxTimestamp,
      feeAsset: Asset,
      fee: TxTimestamp,
      attachment: Array[TxVersion],
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransferTransaction] =
    TransferTransaction(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, proofs).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxTimestamp,
      feeAsset: Asset,
      fee: TxTimestamp,
      attachment: Array[TxVersion],
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, TransferTransaction] =
    create(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, Proofs.empty)
      .map(this.signer.sign(_, signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxTimestamp,
      feeAsset: Asset,
      fee: TxTimestamp,
      attachment: Array[TxVersion],
      timestamp: TxTimestamp
  ): Either[ValidationError, TransferTransaction] =
    signed(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, sender)
}
