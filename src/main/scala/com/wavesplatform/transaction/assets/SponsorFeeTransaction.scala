package com.wavesplatform.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class SponsorFeeTransaction private (sender: PublicKeyAccount,
                                          assetId: ByteStr,
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
        assetId.arr,
        Longs.toByteArray(minSponsoredAssetFee.getOrElse(0)),
        Longs.toByteArray(fee),
        Longs.toByteArray(timestamp)
      )
    )

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      jsonBase() ++ Json.obj(
        "version"              -> version,
        "assetId"              -> assetId.base58,
        "minSponsoredAssetFee" -> minSponsoredAssetFee
      )
    )

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val bytes: Coeval[Array[Byte]]        = Coeval.evalOnce(Bytes.concat(Array(0: Byte, builder.typeId, version), bodyBytes(), proofs.bytes()))

  override def checkedAssets(): Seq[AssetId] = Seq(assetId)
  override def version: Byte                 = SponsorFeeTransaction.version
}

object SponsorFeeTransaction extends TransactionParserFor[SponsorFeeTransaction] with TransactionParser.MultipleVersions {

  val version: Byte                         = 1
  override val typeId: Byte                 = 14
  override val supportedVersions: Set[Byte] = Set(version)

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      (
        if (tx.minSponsoredAssetFee.exists(_ < 0)) {
          Left(ValidationError.NegativeMinFee(tx.minSponsoredAssetFee.get, "asset"))
        } else if (tx.fee <= 0) {
          Left(ValidationError.InsufficientFee())
        } else {
          Right(tx)
        }
      ).foldToTry
    }
  }

  def create(sender: PublicKeyAccount,
             assetId: ByteStr,
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    if (minSponsoredAssetFee.exists(_ < 0)) {
      Left(ValidationError.NegativeMinFee(minSponsoredAssetFee.get, "asset"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(SponsorFeeTransaction(sender, assetId, minSponsoredAssetFee.filter(_ != 0), fee, timestamp, proofs))
    }
  }

  def signed(sender: PublicKeyAccount,
             assetId: ByteStr,
             minSponsoredAssetFee: Option[Long],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, assetId, minSponsoredAssetFee, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: PrivateKeyAccount,
                 assetId: ByteStr,
                 minSponsoredAssetFee: Option[Long],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, assetId, minSponsoredAssetFee, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[SponsorFeeTransaction] = {
    (OneByte(tailIndex(1), "Transaction type") ~
      OneByte(tailIndex(2), "Version") ~
      PublicKeyAccountBytes(tailIndex(3), "Sender's public key") ~
      ByteStrDefinedLength(tailIndex(4), "Asset ID", AssetIdLength) ~
      SponsorFeeOptionLongBytes(tailIndex(5), "Minimal fee in assets*", " * Zero value assume canceling sponsorship") ~
      LongBytes(tailIndex(6), "Fee") ~
      LongBytes(tailIndex(7), "Timestamp") ~
      ProofsBytes(tailIndex(8))).map {
      case (((((((txId, bodyVersion), sender), assetId), minSponsoredAssetFee), fee), timestamp), proofs) =>
        require(txId == typeId, s"Signed tx id is not match")
        require(bodyVersion == version, s"versions are not match ($version, $bodyVersion)")
        SponsorFeeTransaction(
          sender = sender,
          assetId = assetId,
          minSponsoredAssetFee = minSponsoredAssetFee,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
