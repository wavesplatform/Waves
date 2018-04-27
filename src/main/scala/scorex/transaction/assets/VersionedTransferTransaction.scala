package scorex.transaction.assets

import com.google.common.primitives.Bytes
import com.wavesplatform.crypto
import com.wavesplatform.state._
import monix.eval.Coeval
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class VersionedTransferTransaction private (version: Byte,
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

  override val builder: TransactionParser        = VersionedTransferTransaction
  override val assetFee: (Option[AssetId], Long) = (feeAssetId, fee)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Array(builder.typeId, version) ++ bytesBase())

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

object VersionedTransferTransaction extends TransactionParserFor[VersionedTransferTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 4
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      (for {
        parsed <- TransferTransaction.parseBase(bytes, 0)
        (sender, assetIdOpt, feeAssetIdOpt, timestamp, amount, feeAmount, recipient, attachment, end) = parsed
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tt <- VersionedTransferTransaction.create(version,
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
    } yield VersionedTransferTransaction(version, sender, recipient, assetId, amount, timestamp, feeAssetId, feeAmount, attachment, proofs)
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
    create(version, assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
  }
}
