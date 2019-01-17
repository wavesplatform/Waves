package com.wavesplatform.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.transaction._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class SponsorFeeTransaction private (version: Byte,
                                          sender: PublicKeyAccount,
                                          assetId: ByteStr,
                                          minSponsoredAssetFee: Option[Long],
                                          fee: Long,
                                          timestamp: Long,
                                          proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: SponsorFeeTransaction.type = SponsorFeeTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId),
      Array(version),
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(minSponsoredAssetFee.getOrElse(0)),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"              -> version,
      "assetId"              -> assetId.base58,
      "minSponsoredAssetFee" -> minSponsoredAssetFee
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte, builder.typeId, version), bodyBytes(), proofs.bytes()))

  override def checkedAssets(): Seq[AssetId] = Seq(assetId)
}

object SponsorFeeTransaction extends TransactionParserFor[SponsorFeeTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 14
  override val supportedVersions: Set[Byte] = Set(1)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val txId = bytes(0)
      require(txId == typeId, s"Signed tx id is not match")
      val bodyVersion = bytes(1)
      require(version == bodyVersion, s"versions are not match ($version, $bodyVersion)")
      val sender      = PublicKeyAccount(bytes.slice(2, KeyLength + 2))
      val assetId     = ByteStr(bytes.slice(KeyLength + 2, KeyLength + AssetIdLength + 2))
      val minFeeStart = KeyLength + AssetIdLength + 2

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
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (minSponsoredAssetFee.exists(_ < 0)) {
      Left(ValidationError.NegativeMinFee(minSponsoredAssetFee.get, "asset"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(SponsorFeeTransaction(version, sender, assetId, minSponsoredAssetFee.filter(_ != 0), fee, timestamp, proofs))
    }

  def signed(version: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, sender, assetId, minSponsoredAssetFee, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 assetId: ByteStr,
                 minSponsoredAssetFee: Option[Long],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(version, sender, assetId, minSponsoredAssetFee, fee, timestamp, sender)
}
