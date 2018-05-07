package scorex.transaction.data

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state.DataEntry
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.AddressOrAlias
import scorex.transaction.{AssetId, Proofs, ProvenTransaction, ValidationError}

trait DataTransaction extends ProvenTransaction {
  def version: Byte
  def recipient: Option[AddressOrAlias]
  def data: List[DataEntry[_]]
  def fee: Long
  def timestamp: Long
  def proofs: Proofs

  override def assetFee: (Option[AssetId], Long) = (None, fee)

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version"   -> version,
      "data"      -> Json.toJson(data),
      "recipient" -> recipient.map(_.stringRepr)
    )
  }

  def baseBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val recipientBytes =
      if (version > 1) recipient.fold(Array(0: Byte))(a => (1: Byte) +: a.bytes.arr)
      else Array.emptyByteArray

    Bytes.concat(
      sender.publicKey,
      recipientBytes,
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.toBytes).toArray,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }
}

object DataTransaction {
  val MaxEntryCount: Byte = 100

  def validate(supportedVersions: Set[Byte], version: Byte, entries: List[DataEntry[_]], fee: Long): Either[ValidationError, Unit] = {
    for {
      _ <- Either.cond(!supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- Either.cond(entries.lengthCompare(MaxEntryCount) > 0 || entries.exists(!_.valid), (), ValidationError.TooBigArray)
      _ <- Either.cond(fee <= 0, (), ValidationError.InsufficientFee())
    } yield ()
  }
}
