package scorex.transaction

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.crypto
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.TransactionParsers.KeyLength

import scala.util.{Failure, Success, Try}

case class DataTransaction private (version: Byte, sender: PublicKeyAccount, data: List[DataEntry[_]], fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with FastHashId {

  override val builder: TransactionParser        = DataTransaction
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      Shorts.toByteArray(data.size.toShort),
      data.flatMap(_.toBytes).toArray,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee)
    )
  }

  implicit val dataItemFormat: Format[DataEntry[_]] = DataEntry.Format

  override def json(implicit addressScheme: AddressScheme): JsObject =
    jsonBase ++ Json.obj(
      "version" -> version,
      "data"    -> Json.toJson(data)
    )


  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object DataTransaction extends TransactionParserFor[DataTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 12
  override val supportedVersions: Set[Byte] = Set(1)

  val MaxEntryCount: Byte = Byte.MaxValue

  override protected def parseTail(version: Byte, bytes: Array[Byte])(implicit addressScheme: AddressScheme): Try[TransactionT] =
    Try {
      val p0     = KeyLength
      val sender = PublicKeyAccount(bytes.slice(0, p0))

      val entryCount = Shorts.fromByteArray(bytes.drop(p0))
      val (entries, p1) =
        if (entryCount > 0) {
          val parsed = List.iterate(DataEntry.parse(bytes, p0 + 2), entryCount) { case (e, p) => DataEntry.parse(bytes, p) }
          (parsed.map(_._1), parsed.last._2)
        } else (List.empty, p0 + 2)

      val timestamp = Longs.fromByteArray(bytes.drop(p1))
      val feeAmount = Longs.fromByteArray(bytes.drop(p1 + 8))
      val txEi = for {
        proofs <- Proofs.fromBytes(bytes.drop(p1 + 16))
        tx     <- create(version, sender, entries, feeAmount, timestamp, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             data: List[DataEntry[_]],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    if (!supportedVersions.contains(version)) {
      Left(ValidationError.UnsupportedVersion(version))
    } else if (data.lengthCompare(MaxEntryCount) > 0 || data.exists(!_.valid)) {
      Left(ValidationError.TooBigArray)
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(DataTransaction(version, sender, data, feeAmount, timestamp, proofs))
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 data: List[DataEntry[_]],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    create(version, sender, data, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
  }
}
