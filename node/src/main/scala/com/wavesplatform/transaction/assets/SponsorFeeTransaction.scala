package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class SponsorFeeTransaction private (sender: PublicKey,
                                          asset: IssuedAsset,
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
        sender,
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

  override val assetFee: (Asset, Long)    = (Waves, fee)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte, builder.typeId, version), bodyBytes(), proofs.bytes()))

  override def checkedAssets(): Seq[IssuedAsset] = Seq(asset)
  override def version: Byte               = SponsorFeeTransaction.version
}

object SponsorFeeTransaction extends TransactionParserFor[SponsorFeeTransaction] with TransactionParser.MultipleVersions {

  val version: Byte                         = 1
  override val typeId: Byte                 = 14
  override val supportedVersions: Set[Byte] = Set(version)

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      (
        if (tx.minSponsoredAssetFee.exists(_ < 0)) {
          Left(NegativeMinFee(tx.minSponsoredAssetFee.get, "asset"))
        } else if (tx.fee <= 0) {
          Left(InsufficientFee())
        } else {
          Right(tx)
        }
      ).foldToTry
    }
  }

  def create(sender: PublicKey,
             asset: IssuedAsset,
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    if (minSponsoredAssetFee.exists(_ < 0)) {
      Left(NegativeMinFee(minSponsoredAssetFee.get, "asset"))
    } else if (fee <= 0) {
      Left(InsufficientFee())
    } else {
      Right(SponsorFeeTransaction(sender, asset, minSponsoredAssetFee.filter(_ != 0), fee, timestamp, proofs))
    }
  }

  def signed(sender: PublicKey,
             asset: IssuedAsset,
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             signer: PrivateKey): Either[ValidationError, TransactionT] = {
    create(sender, asset, minSponsoredAssetFee, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: KeyPair,
                 asset: IssuedAsset,
                 minSponsoredAssetFee: Option[Long],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, asset, minSponsoredAssetFee, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[SponsorFeeTransaction] = {
    (
      OneByte(tailIndex(1), "Transaction type"),
      OneByte(tailIndex(2), "Version"),
      PublicKeyBytes(tailIndex(3), "Sender's public key"),
      ByteStrDefinedLength(tailIndex(4), "Asset ID", AssetIdLength),
      SponsorFeeOptionLongBytes(tailIndex(5), "Minimal fee in assets*"),
      LongBytes(tailIndex(6), "Fee"),
      LongBytes(tailIndex(7), "Timestamp"),
      ProofsBytes(tailIndex(8))
    ) mapN {
      case (txId, bodyVersion, sender, assetId, minSponsoredAssetFee, fee, timestamp, proofs) =>
        require(txId == typeId, s"Signed tx id is not match")
        require(bodyVersion == version, s"versions are not match ($version, $bodyVersion)")
        SponsorFeeTransaction(
          sender = sender,
          asset = IssuedAsset(assetId),
          minSponsoredAssetFee = minSponsoredAssetFee,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
