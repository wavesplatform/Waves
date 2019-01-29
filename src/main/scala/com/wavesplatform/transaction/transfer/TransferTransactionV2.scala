package com.wavesplatform.transaction.transfer

import com.google.common.primitives.Bytes
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

case class TransferTransactionV2 private (version: Byte,
                                          sender: PublicKeyAccount,
                                          recipient: AddressOrAlias,
                                          assetId: Option[AssetId],
                                          amount: Long,
                                          timestamp: Long,
                                          feeAssetId: Option[AssetId],
                                          fee: Long,
                                          attachment: Array[Byte],
                                          proofs: Proofs)
    extends TransferTransaction
    with ProvenTransaction
    with FastHashId {

  override val builder: TransactionParser     = TransferTransactionV2
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId, version) ++ bytesBase())
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

object TransferTransactionV2 extends TransactionParserFor[TransferTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = TransferTransaction.typeId
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- TransferTransaction.parseBase(bytes, 0)
        (sender, assetIdOpt, feeAssetIdOpt, timestamp, amount, feeAmount, recipient, attachment, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tt <- TransferTransactionV2.create(version,
                                           assetIdOpt.map(ByteStr(_)),
                                           sender,
                                           recipient,
                                           amount,
                                           timestamp,
                                           feeAssetIdOpt.map(ByteStr(_)),
                                           feeAmount,
                                           attachment,
                                           proofs)
      } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             assetId: Option[AssetId],
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- TransferTransaction.validate(amount, feeAmount, attachment)
    } yield TransferTransactionV2(version, sender, recipient, assetId, amount, timestamp, feeAssetId, feeAmount, attachment, proofs)
  }

  def signed(version: Byte,
             assetId: Option[AssetId],
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 assetId: Option[AssetId],
                 sender: PrivateKeyAccount,
                 recipient: AddressOrAlias,
                 amount: Long,
                 timestamp: Long,
                 feeAssetId: Option[AssetId],
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(version, assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, sender)
  }

  val byteDescription: ByteEntity[TransferTransactionV2] = {
    (
      ConstantByte(1, value = 0, name = "Transaction multiple version mark") ~
        ConstantByte(2, value = typeId, name = "Transaction type") ~
        ConstantByte(3, value = 2, name = "Version") ~
        PublicKeyAccountBytes(4, "Sender's public key") ~
        OptionAssetIdBytes(5, "Asset ID") ~
        OptionAssetIdBytes(6, "Fee's asset ID") ~
        LongBytes(7, "Timestamp") ~
        LongBytes(8, "Amount") ~
        LongBytes(9, "Fee") ~
        AddressOrAliasBytes(10, "Recipient") ~
        BytesArrayUndefinedLength(11, "Attachment") ~
        ProofsBytes(12)
    ).map {
      case (((((((((((_, _), version), senderPublicKey), assetId), feeAssetId), timestamp), amount), fee), recipient), attachments), proofs) =>
        TransferTransactionV2(
          version,
          senderPublicKey,
          recipient,
          assetId,
          amount,
          timestamp,
          feeAssetId,
          fee,
          attachments,
          proofs
        )
    }
  }
}
