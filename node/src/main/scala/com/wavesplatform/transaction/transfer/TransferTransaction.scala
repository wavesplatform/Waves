package com.wavesplatform.transaction.transfer

import scala.util.Try

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.serialization.impl.TransferTxSerializer
import com.wavesplatform.transaction.validation._
import com.wavesplatform.transaction.validation.impl.TransferTxValidator
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.JsObject

case class TransferTransaction(
    version: TxVersion,
    sender: PublicKey,
    recipient: AddressOrAlias,
    assetId: Asset,
    amount: TxAmount,
    feeAssetId: Asset,
    fee: TxAmount,
    attachment: ByteStr,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte,
    ethWrappingId: Option[Coeval[ByteStr]] = None
) extends Transaction(TransactionType.Transfer, assetId match {
      case Waves          => Seq()
      case a: IssuedAsset => Seq(a)
    })
    with VersionedTransaction
    with SigProofsSwitch
    with TxWithFee.InCustomAsset
    with PBSince.V3 {

  val bodyBytes: Coeval[TxByteArray] = Coeval.evalOnce(ethWrappingId.fold(TransferTxSerializer.bodyBytes(this))(_ => Array()))
  val bytes: Coeval[TxByteArray]     = Coeval.evalOnce(TransferTxSerializer.toBytes(this))
  final val json: Coeval[JsObject]   = Coeval.evalOnce(TransferTxSerializer.toJson(this))

  override val id: Coeval[ByteStr] = ethWrappingId.getOrElse(FastHashId(this))
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

  override def parseBytes(bytes: TxByteArray): Try[TransferTransaction] = TransferTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: ByteStr,
      timestamp: TxTimestamp,
      proofs: Proofs
  ): Either[ValidationError, TransferTransaction] =
    TransferTransaction(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, proofs, AddressScheme.current.chainId).validatedEither

  def signed(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: TxAmount,
      feeAsset: Asset,
      fee: TxAmount,
      attachment: ByteStr,
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
      attachment: ByteStr,
      timestamp: TxTimestamp
  ): Either[ValidationError, TransferTransaction] =
    signed(version, sender.publicKey, recipient, asset, amount, feeAsset, fee, attachment, timestamp, sender.privateKey)
}
