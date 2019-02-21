package com.wavesplatform.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.AssetId.{Asset, Waves}
import com.wavesplatform.transaction._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class SponsorFeeTransaction private (sender: PublicKeyAccount,
                                          asset: Asset,
                                          minSponsoredAssetFee: Option[Long],
                                          fee: Long,
                                          timestamp: Long,
                                          proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: SponsorFeeTransaction.type = SponsorFeeTransaction

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId),
        Array(version),
        sender.publicKey,
        asset.id.arr,
        Longs.toByteArray(minSponsoredAssetFee.getOrElse(0)),
        Longs.toByteArray(fee),
        Longs.toByteArray(timestamp)
      )
    )

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      jsonBase() ++ Json.obj(
        "version"              -> version,
        "assetId"              -> asset.id.base58,
        "minSponsoredAssetFee" -> minSponsoredAssetFee
      )
    )

  override val assetFee: (AssetId, Long)  = (Waves, fee)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte, builder.typeId, version), bodyBytes(), proofs.bytes()))

  override def checkedAssets(): Seq[AssetId] = Seq(asset)
  override def version: Byte                 = SponsorFeeTransaction.version
}

object SponsorFeeTransaction extends TransactionParserFor[SponsorFeeTransaction] with TransactionParser.MultipleVersions {

  val version: Byte                         = 1
  override val typeId: Byte                 = 14
  override val supportedVersions: Set[Byte] = Set(version)

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val txId = bytes(0)
      require(txId == typeId, s"Signed tx id is not match")
      val bodyVersion = bytes(1)
      require(bodyVersion == version, s"versions are not match ($version, $bodyVersion)")
      val sender      = PublicKeyAccount(bytes.slice(2, KeyLength + 2))
      val asset       = Asset(ByteStr(bytes.slice(KeyLength + 2, KeyLength + AssetIdLength + 2)))
      val minFeeStart = KeyLength + AssetIdLength + 2

      val minFee    = Longs.fromByteArray(bytes.slice(minFeeStart, minFeeStart + 8))
      val fee       = Longs.fromByteArray(bytes.slice(minFeeStart + 8, minFeeStart + 16))
      val timestamp = Longs.fromByteArray(bytes.slice(minFeeStart + 16, minFeeStart + 24))
      val tx = for {
        proofs <- Proofs.fromBytes(bytes.drop(minFeeStart + 24))
        tx     <- SponsorFeeTransaction.create(sender, asset, Some(minFee).filter(_ != 0), fee, timestamp, proofs)
      } yield {
        tx
      }
      tx.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(sender: PublicKeyAccount,
             asset: Asset,
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    if (minSponsoredAssetFee.exists(_ < 0)) {
      Left(ValidationError.NegativeMinFee(minSponsoredAssetFee.get, "asset"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(SponsorFeeTransaction(sender, asset, minSponsoredAssetFee.filter(_ != 0), fee, timestamp, proofs))
    }
  }

  def signed(sender: PublicKeyAccount,
             asset: Asset,
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, asset, minSponsoredAssetFee, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 asset: Asset,
                 minSponsoredAssetFee: Option[Long],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, asset, minSponsoredAssetFee, fee, timestamp, sender)
  }
}
