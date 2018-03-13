package scorex.transaction

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.crypto
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}

import scorex.transaction.TransactionParser.{KeyLength, TransactionType}

import scala.util.{Failure, Success, Try}

case class DataTransaction private(version: Byte,
                                   sender: PublicKeyAccount,
                                   data: List[DataEntry[_]],
                                   fee: Long,
                                   timestamp: Long,
                                   proofs: Proofs) extends ProvenTransaction with FastHashId { ///is it ok for id?
  override val transactionType: TransactionType.Value = TransactionType.DataTransaction

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val dataBytes = Shorts.toByteArray(data.size.toShort) ++ data.flatMap(_.toBytes)
    Bytes.concat(
      Array(transactionType.id.toByte),
      Array(version),
      sender.publicKey,
      dataBytes,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  }

  implicit val dataItemFormat: Format[DataEntry[_]] = DataEntry.Format

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "data" -> Json.toJson(data),
      "version" -> version)
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))
}

object DataTransaction {
  val MaxDataItemCount = Byte.MaxValue

  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    val version = bytes(0)
    val p0 = KeyLength + 1
    val sender = PublicKeyAccount(bytes.slice(1, p0))

    val itemCount = Shorts.fromByteArray(bytes.drop(p0))
    val itemList = List.iterate(DataEntry.parse(bytes, p0 + 2), itemCount) { case (pair, pos) => DataEntry.parse(bytes, pos) }
    val items = itemList.map(_._1)
    Console.err.println("READ " + items)///
    val p1 = itemList.lastOption.map(_._2).getOrElse(p0 + 2)
    val timestamp = Longs.fromByteArray(bytes.drop(p1))
    val feeAmount = Longs.fromByteArray(bytes.drop(p1 + 8))
    val txEi = for {
      proofs <- Proofs.fromBytes(bytes.drop(p1 + 16))
      tx <- create(version, sender, items, feeAmount, timestamp, proofs)
    } yield tx
    txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             data: List[DataEntry[_]],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, DataTransaction] = {
    if (data.lengthCompare(MaxDataItemCount) > 0 || data.exists(! _.valid)) {
      Left(ValidationError.TooBigArray) ///better diagnostics
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
                 timestamp: Long): Either[ValidationError, DataTransaction] = {
    create(version, sender, data, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
  }
}
