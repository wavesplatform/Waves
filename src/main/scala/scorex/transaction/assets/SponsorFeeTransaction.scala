package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class SponsorFeeTransaction private (version: Byte,
                                          sender: PublicKeyAccount,
                                          assetId: ByteStr,
                                          minAssetFee: Option[Long],
                                          fee: Long,
                                          timestamp: Long,
                                          proofs: Proofs)
    extends ProvenTransaction
    with FastHashId {

  override val builder: SponsorFeeTransaction.type = SponsorFeeTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId),
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(minAssetFee.getOrElse(0)),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"     -> version,
      "assetId"     -> assetId.base58,
      "minAssetFee" -> minAssetFee
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte, builder.typeId, version), bodyBytes(), proofs.bytes()))
}

object SponsorFeeTransaction extends TransactionParserFor[SponsorFeeTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 14
  override val supportedVersions: Set[Byte] = Set(1)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val txId = bytes(0)
      require(txId == typeId, s"Signed tx id is not match")
      val sender      = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
      val assetId     = ByteStr(bytes.slice(KeyLength + 1, KeyLength + AssetIdLength + 1))
      val minFeeStart = KeyLength + AssetIdLength + 1

      val minFee    = Longs.fromByteArray(bytes.slice(minFeeStart, minFeeStart + 8))
      val fee       = Longs.fromByteArray(bytes.slice(minFeeStart + 8, minFeeStart + 16))
      val timestamp = Longs.fromByteArray(bytes.slice(minFeeStart + 16, minFeeStart + 24))
      val tx = for {
        proofs <- Proofs.fromBytes(bytes.drop(minFeeStart + 24))
        tx     <- SponsorFeeTransaction.create(version, sender, assetId, Some(minFee).filter(_ != 0), fee, timestamp, proofs)
      } yield {
        tx
      }
      tx.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             minAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (minAssetFee.exists(_ <= 0)) {
      Left(ValidationError.NegativeMinFee(minAssetFee.get, "asset"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(SponsorFeeTransaction(version, sender, assetId, minAssetFee, fee, timestamp, proofs))
    }

  def create(version: Byte,
             sender: PrivateKeyAccount,
             assetId: ByteStr,
             minAssetFee: Option[Long],
             fee: Long,
             timestamp: Long): Either[ValidationError, TransactionT] =
    create(version, sender, assetId, minAssetFee, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
}
