package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.serialization.Deser
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
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val timestampBytes  = Longs.toByteArray(timestamp)
    val assetIdBytes    = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val amountBytes     = Longs.toByteArray(amount)
    val feeBytes        = Longs.toByteArray(fee)

    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      assetIdBytes,
      timestampBytes,
      amountBytes,
      feeAssetIdBytes,
      feeBytes,
      recipient.bytes.arr,
      Deser.serializeArray(attachment)
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "recipient"  -> recipient.stringRepr,
      "assetId"    -> assetId.map(_.base58),
      "feeAssetId" -> feeAssetId.map(_.base58),
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

object VersionedTransferTransaction extends TransactionParserFor[VersionedTransferTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 4
  override val supportedVersions: Set[Byte] = Set(2)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val sender              = PublicKeyAccount(bytes.slice(0, KeyLength))
      val (assetIdOpt, s0)    = Deser.parseByteArrayOption(bytes, KeyLength, AssetIdLength)
      val timestamp           = Longs.fromByteArray(bytes.slice(s0, s0 + 8))
      val amount              = Longs.fromByteArray(bytes.slice(s0 + 8, s0 + 16))
      val (feeAssetIdOpt, s1) = Deser.parseByteArrayOption(bytes, s0 + 16, AssetIdLength)
      val feeAmount           = Longs.fromByteArray(bytes.slice(s1, s1 + 8))

      (for {
        recRes <- AddressOrAlias.fromBytes(bytes, s1 + 8)
        (recipient, recipientEnd) = recRes
        (attachment, attachEnd)   = Deser.parseArraySize(bytes, recipientEnd)
        proofs <- Proofs.fromBytes(bytes.drop(attachEnd))
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
