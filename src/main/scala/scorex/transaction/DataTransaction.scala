package scorex.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.DataTransaction.Data
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

case class DataTransaction private(sender: PublicKeyAccount,
                                   data: Data,
                                   fee: Long,
                                   timestamp: Long,
                                   signature: ByteStr) extends SignedTransaction with FastHashId { ///is it ok for id?
  override val transactionType: TransactionType.Value = TransactionType.DataTransaction

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  ///version, proofs

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(
    Array(transactionType.id.toByte),
    sender.publicKey,
    ///data.toSeq.sortBy(_._1),
    Longs.toByteArray(timestamp),
    Longs.toByteArray(fee)))

  implicit val typedValueFormat = DataTransaction.TypedValueFormat

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj("data" -> Json.toJson(data))
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))
}

object DataTransaction {
  val MaxKeySize = Byte.MaxValue ///unsigned?
  val MaxValueSize = Short.MaxValue
  type Data = Map[String, TypedValue[_]] ///move somewhere

  sealed abstract class TypedValue[T](typestring: String, marshaller: T => JsValue, value: T) {
    def write: JsObject = Json.obj("type" -> typestring, "value" -> marshaller(value))
  }

  case class IntegerValue(value: Long) extends TypedValue[Long]("int", JsNumber(_), value)
  case class BooleanValue(value: Boolean) extends TypedValue[Boolean]("bool", JsBoolean.apply, value)
  case class ByteStrValue(value: ByteStr) extends TypedValue[ByteStr]("bin", bstr => JsString(Base58.encode(bstr.arr)), value)

  implicit object TypedValueFormat extends Format[TypedValue[_]] {
    def reads(jsv: JsValue): JsResult[TypedValue[_]] = {
      val valueTry = (jsv \ "type", jsv \ "value") match {
        case (JsDefined(JsString("int")), JsDefined(JsNumber(n))) => Success(IntegerValue(n.toLong))
        case (JsDefined(JsString("bool")), JsDefined(JsBoolean(b))) => Success(BooleanValue(b))
        case (JsDefined(JsString("bin")), JsDefined(JsString(base58))) => Base58.decode(base58).map(arr => ByteStrValue(ByteStr(arr)))
        case (JsDefined(t), JsDefined(v)) => Failure(new Throwable(s"Value $v does not match type $t"))
        case _ => Failure(new Throwable("Malformed JSON"))
      }
      valueTry.fold(ex => JsError(ex.getMessage), value => JsSuccess(value))
    }

    def writes(tv: TypedValue[_]): JsValue = tv.write
  }

  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val data = Map("parsed from binary" -> BooleanValue(true)) ///
    val s0 = KeyLength
    val timestamp = Longs.fromByteArray(bytes.slice(s0, s0 + 8))
    val feeAmount = Longs.fromByteArray(bytes.slice(s0 + 8, s0 + 16))
    val signature = ByteStr(bytes.slice(s0 + 16, s0 + 16 + SignatureLength))

    DataTransaction.create(sender, data, feeAmount, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             data: Data,
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
             data: Data,
             feeAmount: Long,
             timestamp: Long): Either[ValidationError, DataTransaction] = {
    create(sender, data, feeAmount, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
  }
}
