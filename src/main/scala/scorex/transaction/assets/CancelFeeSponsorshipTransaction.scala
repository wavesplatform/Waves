package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class CancelFeeSponsorshipTransaction private (version: Byte,
                                                    sender: PublicKeyAccount,
                                                    assetId: ByteStr,
                                                    fee: Long,
                                                    timestamp: Long,
                                                    proofs: Proofs)
    extends ProvenTransaction
    with FastHashId {

  override val builder: CancelFeeSponsorshipTransaction.type = CancelFeeSponsorshipTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId),
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version" -> version,
      "assetId" -> assetId.base58
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte, builder.typeId, version), bodyBytes(), proofs.bytes()))
}

object CancelFeeSponsorshipTransaction extends TransactionParserFor[CancelFeeSponsorshipTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 15
  override val supportedVersions: Set[Byte] = Set(1)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val txId = bytes(0)
      require(txId == typeId, s"Signed tx id is not match")
      val sender   = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
      val assetId  = ByteStr(bytes.slice(KeyLength + 1, KeyLength + AssetIdLength + 1))
      val feeStart = KeyLength + AssetIdLength + 1

      val fee       = Longs.fromByteArray(bytes.slice(feeStart, feeStart + 8))
      val timestamp = Longs.fromByteArray(bytes.slice(feeStart + 8, feeStart + 16))
      val tx = for {
        proofs <- Proofs.fromBytes(bytes.drop(feeStart + 16))
        tx     <- CancelFeeSponsorshipTransaction.create(version, sender, assetId, fee, timestamp, proofs)
      } yield {
        tx
      }
      tx.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(CancelFeeSponsorshipTransaction(version, sender, assetId, fee, timestamp, proofs))
    }

  def create(version: Byte, sender: PrivateKeyAccount, assetId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] =
    create(version, sender, assetId, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
}
