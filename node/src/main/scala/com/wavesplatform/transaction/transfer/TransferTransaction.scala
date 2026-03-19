package com.wavesplatform.transaction.transfer

import com.wavesplatform.account.*
import com.wavesplatform.common.state.ByteStr
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
    ),
      ProvenTransaction,
      TransferTransactionLike,
      Versioned.ToV3,
      FastHashId,
      HasSignature,
      TxWithFee.InCustomAsset,
      PBSince.V3 {
  override type T = TransferTransaction

  val bodyBytes: Coeval[TxByteArray] = Coeval.evalOnce(TransferTxSerializer.bodyBytes(this))
  val bytes: Coeval[TxByteArray]     = Coeval.evalOnce(TransferTxSerializer.toBytes(this))
  final val json: Coeval[JsObject]   = Coeval.evalOnce(TransferTxSerializer.toJson(this))

  override def addProof(proof: ByteStr): TransferTransaction = copy(proofs = this.proofs.add(proof))
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
}
