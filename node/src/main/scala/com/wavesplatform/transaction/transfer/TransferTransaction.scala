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

import scala.util.Try

case class TransferTransaction(
    version: TxVersion,
    sender: PublicKey,
    recipient: AddressOrAlias,
    assetId: Asset,
    amount: TxAmount,
    feeAssetId: Asset,
    fee: TxAmount,
    attachment: Option[Attachment],
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends VersionedTransaction
    with SigProofsSwitch
    with FastHashId
    with TxWithFee.InCustomAsset
    with LegacyPBSwitch.V3 {

  override val typeId: TxType = TransferTransaction.typeId

  val bodyBytes: Coeval[TxByteArray] = Coeval.evalOnce(TransferTransaction.serializer.bodyBytes(this))
  val bytes: Coeval[TxByteArray]     = Coeval.evalOnce(TransferTransaction.serializer.toBytes(this))
  final val json: Coeval[JsObject]   = Coeval.evalOnce(TransferTransaction.serializer.toJson(this))

  override def checkedAssets: Seq[IssuedAsset] = assetId match {
    case a: IssuedAsset => Seq(a)
    case Waves          => Nil
  }

  override def builder: TransactionParser = TransferTransaction
}

object TransferTransaction extends TransactionParser {
  type TransactionT = TransferTransaction

  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  val typeId: TxType                    = 4: Byte
  val supportedVersions: Set[TxVersion] = Set(1, 2, 3)

  implicit val validator: TxValidator[TransferTransaction] = TransferTxValidator

  implicit def sign(tx: TransferTransaction, privateKey: PrivateKey): TransferTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  val serializer = TransferTxSerializer

  override def parseBytes(bytes: TxByteArray): Try[TransferTransaction] = serializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: Option[Attachment],
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransferTransaction] =
    TransferTransaction(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, proofs, recipient.chainId).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: Option[Attachment],
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
      attachment: Option[Attachment],
      timestamp: TxTimestamp
  ): Either[ValidationError, TransferTransaction] =
    signed(version, sender.publicKey, recipient, asset, amount, feeAsset, fee, attachment, timestamp, sender.privateKey)
}
