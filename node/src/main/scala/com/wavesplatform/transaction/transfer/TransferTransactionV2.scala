package com.wavesplatform.transaction.transfer

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class TransferTransactionV2 private (sender: PublicKey,
                                          recipient: AddressOrAlias,
                                          assetId: Asset,
                                          amount: Long,
                                          timestamp: Long,
                                          feeAssetId: Asset,
                                          fee: Long,
                                          attachment: Array[Byte],
                                          proofs: Proofs)
    extends TransferTransaction
    with ProvenTransaction
    with FastHashId {

  override val builder: TransactionParser     = TransferTransactionV2
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId, version) ++ bytesBase())
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 2
}

object TransferTransactionV2 extends TransactionParserFor[TransferTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = TransferTransaction.typeId
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      TransferTransaction
        .validate(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(assetId: Asset,
             sender: PublicKey,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Asset,
             feeAmount: Long,
             attachment: Array[Byte],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- TransferTransaction.validate(amount, assetId, feeAmount, feeAssetId, attachment)
    } yield TransferTransactionV2(sender, recipient, assetId, amount, timestamp, feeAssetId, feeAmount, attachment, proofs)
  }

  def signed(assetId: Asset,
             sender: PublicKey,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Asset,
             feeAmount: Long,
             attachment: Array[Byte],
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(assetId: Asset,
                 sender: KeyPair,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 feeAssetId: Asset,
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, sender)
  }

  val byteTailDescription: ByteEntity[TransferTransactionV2] = {
    (
      PublicKeyBytes(tailIndex(1), "Sender's public key"),
      OptionBytes(tailIndex(2), "Asset ID", AssetIdBytes(tailIndex(2), "Asset ID"), "flag (1 - asset, 0 - Waves)"),
      OptionBytes(tailIndex(3), "Fee's asset ID", AssetIdBytes(tailIndex(3), "Fee's asset ID"), "flag (1 - asset, 0 - Waves)"),
      LongBytes(tailIndex(4), "Timestamp"),
      LongBytes(tailIndex(5), "Amount"),
      LongBytes(tailIndex(6), "Fee"),
      AddressOrAliasBytes(tailIndex(7), "Recipient"),
      BytesArrayUndefinedLength(tailIndex(8), "Attachment", TransferTransaction.MaxAttachmentSize),
      ProofsBytes(tailIndex(9))
    ) mapN {
      case (senderPublicKey, assetId, feeAssetId, timestamp, amount, fee, recipient, attachments, proofs) =>
        TransferTransactionV2(
          sender = senderPublicKey,
          recipient = recipient,
          assetId = assetId.getOrElse(Waves),
          amount = amount,
          timestamp = timestamp,
          feeAssetId = feeAssetId.getOrElse(Waves),
          fee = fee,
          attachment = attachments,
          proofs = proofs
        )
    }
  }
}
