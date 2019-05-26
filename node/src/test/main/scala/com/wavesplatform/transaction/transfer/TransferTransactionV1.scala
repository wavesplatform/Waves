package com.wavesplatform.transaction.transfer

import cats.implicits._
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class TransferTransactionV1 private (assetId: Asset,
                                          sender: PublicKey,
                                          recipient: AddressOrAlias,
                                          amount: Long,
                                          timestamp: Long,
                                          feeAssetId: Asset,
                                          fee: Long,
                                          attachment: Array[Byte],
                                          signature: ByteStr)
    extends TransferTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: TransactionParser     = TransferTransactionV1
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId) ++ bytesBase())
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))
  override val version: Byte                  = 1: Byte
}

object TransferTransactionV1 extends TransactionParserFor[TransferTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = TransferTransaction.typeId

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
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    TransferTransaction
      .validate(amount, assetId, feeAmount, feeAssetId, attachment)
      .map(_ => TransferTransactionV1(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, signature))
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
    create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(signer, unsigned.bodyBytes())))
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

  val byteTailDescription: ByteEntity[TransferTransactionV1] = {
    (
      SignatureBytes(tailIndex(1), "Signature"),
      ConstantByte(tailIndex(2), value = typeId, name = "Transaction type"),
      PublicKeyBytes(tailIndex(3), "Sender's public key"),
      OptionBytes[IssuedAsset](tailIndex(4), "Asset ID", AssetIdBytes(tailIndex(4), "Asset ID"), "flag (1 - asset, 0 - Waves)"),
      OptionBytes[IssuedAsset](tailIndex(5), "Fee's asset ID", AssetIdBytes(tailIndex(5), "Fee's asset ID"), "flag (1 - asset, 0 - Waves)"),
      LongBytes(tailIndex(6), "Timestamp"),
      LongBytes(tailIndex(7), "Amount"),
      LongBytes(tailIndex(8), "Fee"),
      AddressOrAliasBytes(tailIndex(9), "Recipient"),
      BytesArrayUndefinedLength(tailIndex(10), "Attachment", TransferTransaction.MaxAttachmentSize)
    ) mapN {
      case (signature, txId, senderPublicKey, assetId, feeAssetId, timestamp, amount, fee, recipient, attachments) =>
        require(txId == typeId, s"Signed tx id is not match")
        TransferTransactionV1(
          assetId = assetId.getOrElse(Waves),
          sender = senderPublicKey,
          recipient = recipient,
          amount = amount,
          timestamp = timestamp,
          feeAssetId = feeAssetId.getOrElse(Waves),
          fee = fee,
          attachment = attachments,
          signature = signature
        )
    }
  }
}
