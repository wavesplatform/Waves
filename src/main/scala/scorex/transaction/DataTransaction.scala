package scorex.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

case class DataTransaction private(sender: PublicKeyAccount,
                                   data: Map[String, String], /// [String, Any]
                                   fee: Long,
                                   timestamp: Long,
                                   signature: ByteStr) extends SignedTransaction {
  override val transactionType: TransactionType.Value = TransactionType.DataTransaction

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  ///version, proofs

  val toSign: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      Array(transactionType.id.toByte),
      sender.publicKey,
      ///data.toSeq.sortBy(_._1),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee))
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj("data" -> Json.toJson(data))
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(toSign(), signature.arr))
}

object DataTransaction {
  val MaxKeySize = Byte.MaxValue ///unsigned?
  val MaxValueSize = Short.MaxValue

  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    import EllipticCurveImpl._

    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val data = Map[String, String]("parsed" -> "yes", "reparsed" -> "yes again") ///
    val s0 = KeyLength
    val timestamp = Longs.fromByteArray(bytes.slice(s0, s0 + 8))
    val feeAmount = Longs.fromByteArray(bytes.slice(s0 + 8, s0 + 16))
    val signature = ByteStr(bytes.slice(s0 + 16, s0 + 16 + SignatureLength))

    DataTransaction.create(sender, data, feeAmount, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             data: Map[String, String],
             feeAmount: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, DataTransaction] = {
    if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(DataTransaction(sender, data, feeAmount, timestamp, signature))
    }
  }

  def create(sender: PrivateKeyAccount,
             data: Map[String, String],
             feeAmount: Long,
             timestamp: Long): Either[ValidationError, DataTransaction] = {
    create(sender, data, feeAmount, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign())))
    }
  }
}
