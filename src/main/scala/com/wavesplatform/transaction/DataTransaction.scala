package com.wavesplatform.transaction

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import monix.eval.Coeval
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

case class DataTransaction private (sender: PublicKeyAccount, data: List[DataEntry[_]], fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = DataTransaction
  override val assetFee: (Asset, Long)    = (Waves, fee)
  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce {
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

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "data"    -> Json.toJson(data)
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 1
}

object DataTransaction extends TransactionParserFor[DataTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 12
  override val supportedVersions: Set[Byte] = Set(1)

  val MaxBytes      = 150 * 1024
  val MaxEntryCount = 100

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
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
        tx     <- create(sender, entries, feeAmount, timestamp, proofs)
      } yield tx
      txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(sender: PublicKeyAccount,
             data: List[DataEntry[_]],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    if (data.lengthCompare(MaxEntryCount) > 0 || data.exists(!_.valid)) {
      Left(ValidationError.TooBigArray)
    } else if (data.exists(_.key.isEmpty)) {
      Left(ValidationError.GenericError("Empty key found"))
    } else if (data.map(_.key).distinct.lengthCompare(data.size) < 0) {
      Left(ValidationError.GenericError("Duplicate keys found"))
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      val tx = DataTransaction(sender, data, feeAmount, timestamp, proofs)
      Either.cond(tx.bytes().length <= MaxBytes, tx, ValidationError.TooBigArray)
    }
  }

  def signed(sender: PublicKeyAccount,
             data: List[DataEntry[_]],
             feeAmount: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, data, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: PrivateKeyAccount, data: List[DataEntry[_]], feeAmount: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, data, feeAmount, timestamp, sender)
  }
}
