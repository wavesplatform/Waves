package com.wavesplatform.transaction.transfer

import com.wavesplatform.account._
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.TransferTxSerializer
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

  override val typeId: TxType = TransferTransaction.typeId

  // TODO: Rework caching
  val bodyBytes: Coeval[TxByteArray] = Coeval.evalOnce(TransferTransaction.serializer.bodyBytes(this))
  val bytes: Coeval[TxByteArray]     = Coeval.evalOnce(TransferTransaction.serializer.toBytes(this))
  final val json: Coeval[JsObject]   = Coeval.evalOnce(TransferTransaction.serializer.toJson(this))

  override def checkedAssets: Seq[IssuedAsset] = assetId match {
    case a: IssuedAsset => Seq(a)
    case Waves          => Nil
  }

  override def builder: TransactionParserLite = TransferTransaction
}

object TransferTransaction extends TransactionParserLite {
  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  implicit val validator: TxValidator[TransferTransaction] = TransferTxValidator
  val serializer                                           = TransferTxSerializer

  val typeId: TxType = 4.toByte

  override def supportedVersions: Set[TxVersion] = Set(1.toByte, 2.toByte)

  override type TransactionT = TransferTransaction

  override def classTag: ClassTag[TransferTransaction] = ClassTag(classOf[TransferTransaction])

  implicit def sign(tx: TransferTransaction, privateKey: PrivateKey): TransferTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: TxByteArray): Try[TransferTransaction] = serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: TxByteArray,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransferTransaction] =
    TransferTransaction(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, proofs).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: TxByteArray,
      timestamp: TxTimestamp,
      signer: PrivateKey
  ): Either[ValidationError, TransferTransaction] =
    create(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, Proofs.empty).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: TxByteArray,
      timestamp: TxTimestamp
  ): Either[ValidationError, TransferTransaction] =
    signed(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, sender)
}
