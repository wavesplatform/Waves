package scorex.transaction

import java.nio.charset.StandardCharsets

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.crypto
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.DataTransaction.DataItem
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction.ValidationError.Validation

import scala.util.{Failure, Success, Try}

case class DataTransaction private(version: Byte,
                                   sender: PublicKeyAccount,
                                   data: List[DataItem[_]],
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

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "data" -> Json.toJson(data),
      "version" -> version)
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))
}

object DataTransaction {
  val MaxDataItemCount = Byte.MaxValue
  val MaxKeySize = Byte.MaxValue
  val MaxValueSize = Short.MaxValue

  private val UTF8 = StandardCharsets.UTF_8

  case class DataItemSpec(key: String, `type`: String, value: String) {///converge w/ DataItem
    def parse: Validation[DataItem[_]] = {
      val typedValueEi = `type` match {
        case "integer" => Try(java.lang.Long.parseLong(value)).map(IntegerDataItem(key, _))
        case "boolean" => Try(java.lang.Boolean.parseBoolean(value)).map(BooleanDataItem(key, _))
        case "binary" => Base58.decode(value).map(BinaryDataItem(key, _))
      }
      typedValueEi.toEither.left.map(ValidationError.GenericError(_))
    }
  }

  implicit val dataItemReads: Reads[DataItemSpec] = Json.reads[DataItemSpec]

  sealed abstract class DataItem[T](val key: String, val value: T) {
    def valueBytes: Array[Byte]

    def toBytes: Array[Byte] = {
      val keyBytes = key.getBytes(UTF8)
      Bytes.concat(Array(keyBytes.length.toByte), keyBytes, valueBytes)
    }

    def toJson: JsObject = Json.obj("key" -> key)

    def valid: Boolean = key.getBytes(UTF8).length <= MaxKeySize
  }

  object DataItem {
    object Type extends Enumeration {
      val Integer = Value(0)
      val Boolean = Value(1)
      val Binary = Value(2)
    }

    def parse(bytes: Array[Byte], p: Int): (DataItem[_], Int) = {
      val keyLength = bytes(p)
      val key = new String(bytes, p + 1, keyLength, UTF8)
      parse(key, bytes, p + 1 + keyLength)
    }

    def parse(key: String, bytes: Array[Byte], p: Int): (DataItem[_], Int) = {
      bytes(p) match {
        case t if t == Type.Integer.id => (IntegerDataItem(key, Longs.fromByteArray(bytes.drop(p + 1))), p + 9)
        case t if t == Type.Boolean.id => (BooleanDataItem(key, bytes(p + 1) != 0), p + 2)
        case t if t == Type.Binary.id =>
          val (blob, p1) = Deser.parseArraySize(bytes, p + 1)
          (BinaryDataItem(key, blob), p1)
        /// case _ => Validation
      }
    }
  }

  case class IntegerDataItem(k: String, v: Long) extends DataItem[Long](k, v) {
    override def valueBytes: Array[Byte] = DataItem.Type.Integer.id.toByte +: Longs.toByteArray(value)

    override def toJson: JsObject = super.toJson + ("type" -> JsString("integer")) + ("value" -> JsString(value.toString))
  }

  case class BooleanDataItem(k: String, v: Boolean) extends DataItem[Boolean](k, v) {
    override def valueBytes: Array[Byte] = Array(DataItem.Type.Boolean.id, if (value) 1 else 0).map(_.toByte)

    override def toJson: JsObject = super.toJson + ("type" -> JsString("boolean")) + ("value" -> JsString(value.toString))
  }

  case class BinaryDataItem(k: String, v: Array[Byte]) extends DataItem[Array[Byte]](k, v) {
    override def valueBytes: Array[Byte] = DataItem.Type.Binary.id.toByte +: Deser.serializeArray(value)

    override def toJson: JsObject = super.toJson + ("type" -> JsString("binary")) + ("value" -> JsString(Base58.encode(value)))

    override def valid: Boolean = value.length <= MaxValueSize
  }

  implicit val dataItemWrites: Writes[DataItem[_]] = _.toJson

//    implicit object Format extends Format[TypedValue[_]] {
//      def reads(jsv: JsValue): JsResult[TypedValue[_]] = {
//        val valueTry = (jsv \ "type", jsv \ "value") match {
//          case (JsDefined(JsString("int")), JsDefined(JsNumber(n))) => Success(IntegerValue(n.toLong))
//          case (JsDefined(JsString("bool")), JsDefined(JsBoolean(b))) => Success(BooleanValue(b))
//          case (JsDefined(JsString("bin")), JsDefined(JsString(base58))) => Base58.decode(base58).map(arr => BinaryValue(ByteStr(arr)))
//          case (JsDefined(t), JsDefined(v)) => Failure(new Throwable(s"Value $v does not match type $t"))
//          case _ => Failure(new Throwable("Malformed JSON"))
//        }
//        valueTry.fold(ex => JsError(ex.getMessage), value => JsSuccess(value))
//      }
//
//      def writes(tv: TypedValue[_]): JsValue = tv.toJson
//    }

  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    val version = bytes(0)
    val p0 = KeyLength + 1
    val sender = PublicKeyAccount(bytes.slice(1, p0))

    val itemCount = Shorts.fromByteArray(bytes.slice(p0, p0 + 2))
    val itemList = List.iterate(DataItem.parse(bytes, p0 + 2), itemCount) { case (pair, pos) => DataItem.parse(bytes, pos) }
    val items = itemList.map(_._1)
    Console.err.println("READ " + items)///
    val p1 = itemList.lastOption.map(_._2).getOrElse(p0 + 2)
    val timestamp = Longs.fromByteArray(bytes.slice(p1, p1 + 8))
    val feeAmount = Longs.fromByteArray(bytes.slice(p1 + 8, p1 + 16))
    val txEi = for {
      proofs <- Proofs.fromBytes(bytes.drop(p1 + 16))
      tx <- create(version, sender, items, feeAmount, timestamp, proofs)
    } yield tx
    txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             data: List[DataItem[_]],
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
                 data: List[DataItem[_]],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, DataTransaction] = {
    create(version, sender, data, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
  }
}
