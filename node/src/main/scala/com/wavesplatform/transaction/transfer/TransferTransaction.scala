package com.wavesplatform.transaction.transfer

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.serialization.impl.TransferTxSerializer
import com.wavesplatform.transaction.validation.*
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
    amount: TxPositiveAmount,
    feeAssetId: Asset,
    fee: TxPositiveAmount,
    attachment: ByteStr,
    timestamp: TxTimestamp,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(
      TransactionType.Transfer,
      assetId match {
        case Waves          => Seq()
        case a: IssuedAsset => Seq(a)
      }
    )
    with TransferTransactionLike
    with VersionedTransaction.ToV3
    with FastHashId
    with SigProofsSwitch
    with TxWithFee.InCustomAsset
    with PBSince.V3 {

  val bodyBytes: Coeval[TxByteArray] = Coeval.evalOnce(TransferTxSerializer.bodyBytes(this))
  val bytes: Coeval[TxByteArray]     = Coeval.evalOnce(TransferTxSerializer.toBytes(this))
  final val json: Coeval[JsObject]   = Coeval.evalOnce(TransferTxSerializer.toJson(this))
}

trait TransferTransactionLike extends TransactionBase with Authorized {
  val sender: PublicKey
  val recipient: AddressOrAlias
  val assetId: Asset
  val amount: TxPositiveAmount
  val attachment: ByteStr
}

object TransferTransaction extends TransactionParser {
  type TransactionT = TransferTransaction

  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  val typeId: TxType = 4: Byte

  implicit val validator: TxValidator[TransferTransaction] = TransferTxValidator

  implicit def sign(tx: TransferTransaction, privateKey: PrivateKey): TransferTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: TxByteArray): Try[TransferTransaction] = TransferTxSerializer.parseBytes(bytes)

  def create(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: ByteStr,
      timestamp: TxTimestamp,
      proofs: Proofs,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransferTransaction] =
    for {
      amount <- TxPositiveAmount(amount)(TxValidationError.NonPositiveAmount(amount, asset.maybeBase58Repr.getOrElse("waves")))
      fee    <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      tx     <- TransferTransaction(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, proofs, chainId).validatedEither
    } yield tx

  def signed(
      version: TxVersion,
      sender: PublicKey,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: ByteStr,
      timestamp: TxTimestamp,
      signer: PrivateKey,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransferTransaction] =
    create(version, sender, recipient, asset, amount, feeAsset, fee, attachment, timestamp, Proofs.empty, chainId).map(_.signWith(signer))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      recipient: AddressOrAlias,
      asset: Asset,
      amount: Long,
      feeAsset: Asset,
      fee: Long,
      attachment: ByteStr,
      timestamp: TxTimestamp,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, TransferTransaction] =
    signed(version, sender.publicKey, recipient, asset, amount, feeAsset, fee, attachment, timestamp, sender.privateKey, chainId)
}
