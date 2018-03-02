package scorex.transaction

import java.nio.charset.Charset

import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.crypto
import com.wavesplatform.state2._
import monix.eval.Coeval
import play.api.libs.json._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.DataTransaction.ParsedItem
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction.ValidationError.Validation

import scala.util.{Failure, Success, Try}

case class DataTransaction private(version: Byte,
                                   sender: PublicKeyAccount,
                                   data: List[ParsedItem],
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
  val MaxKeySize = Byte.MaxValue ///unsigned?
  val MaxValueSize = Short.MaxValue
  type Data = Map[String, TypedValue[_]] ///move somewhere, make class?

  private val UTF8 = Charset.forName("UTF-8")

  object Type extends Enumeration {
    val Integer = Value(0)
    val Boolean = Value(1)
    val Binary = Value(2)
  }

  case class Item(key: String, `type`: String, value: String) {
    def parse: Validation[ParsedItem] =
      Try(java.lang.Long.parseLong(value))
        .map(n => ParsedItem(key, IntegerValue(n))) ///Ints only
        .toEither.left.map(ex => ValidationError.GenericError(ex))
  }

  implicit val itemFormat: Format[Item] = Json.format

  case class ParsedItem(key: String, value: TypedValue[_]) {
    def toBytes: Array[Byte] = Bytes.concat(
      Array(key.length.toByte),
      key.getBytes(UTF8),
      value.toBytes)
  }

  implicit val parsedItemWrites: Writes[ParsedItem] = (item: ParsedItem) =>
    Json.obj("key" -> item.key) ++ item.value.toJson

  def parseItem(b: Array[Byte], p: Int): (ParsedItem, Int) = {
    val keyLength = b(p)
    val s0 = p + 1 + keyLength
    val key = new String(b.slice(p + 1, s0), UTF8)
    val (tv, p1) = TypedValue.fromBytes(b, s0)
    (ParsedItem(key, tv), p1)
  }

  sealed abstract class TypedValue[T](typestring: String, marshaller: T => JsValue, value: T) {
    def toJson: JsObject = Json.obj("type" -> typestring, "value" -> marshaller(value))
    def toBytes: Array[Byte]
  }

  object TypedValue {
    def fromBytes(b: Array[Byte], position: Int): (TypedValue[_], Int) = b(position) match {
      case t if t == Type.Integer.id => (IntegerValue(Longs.fromByteArray(b.drop(position + 1))), position + 9)
      case t if t == Type.Boolean.id => (BooleanValue(b(position + 1) != 0), position + 2)
      case t if t == Type.Binary.id =>
        val (blob, pos) = Deser.parseArraySize(b, position + 1)
        (BinaryValue(ByteStr(blob)), pos)
    }

    implicit object Format extends Format[TypedValue[_]] {
      def reads(jsv: JsValue): JsResult[TypedValue[_]] = {
        val valueTry = (jsv \ "type", jsv \ "value") match {
          case (JsDefined(JsString("int")), JsDefined(JsNumber(n))) => Success(IntegerValue(n.toLong))
          case (JsDefined(JsString("bool")), JsDefined(JsBoolean(b))) => Success(BooleanValue(b))
          case (JsDefined(JsString("bin")), JsDefined(JsString(base58))) => Base58.decode(base58).map(arr => BinaryValue(ByteStr(arr)))
          case (JsDefined(t), JsDefined(v)) => Failure(new Throwable(s"Value $v does not match type $t"))
          case _ => Failure(new Throwable("Malformed JSON"))
        }
        valueTry.fold(ex => JsError(ex.getMessage), value => JsSuccess(value))
      }

      def writes(tv: TypedValue[_]): JsValue = tv.toJson
    }
  }

  import Type._
  case class IntegerValue(value: Long) extends TypedValue[Long]("int", JsNumber(_), value) {
    override def toBytes: Array[Byte] = Array(Integer.id.toByte) ++ Longs.toByteArray(value)
  }

  case class BooleanValue(value: Boolean) extends TypedValue[Boolean]("bool", JsBoolean.apply, value) {
    override def toBytes: Array[Byte] = Array(Boolean.id.toByte) ++ Array((if (value) 1 else 0): Byte)
  }

  case class BinaryValue(value: ByteStr) extends TypedValue[ByteStr]("bin", bstr => JsString(Base58.encode(bstr.arr)), value) {///accept Array[Byte]?
    override def toBytes: Array[Byte] = Array(Binary.id.toByte) ++ Deser.serializeArray(value.arr)
  }

  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    val version = bytes(0)
    val s0 = KeyLength + 1
    val sender = PublicKeyAccount(bytes.slice(1, s0))

    val itemCount = Shorts.fromByteArray(bytes.slice(s0, s0 + 2))
    val itemList = List.iterate(parseItem(bytes, s0 + 2), itemCount) { case (pair, pos) => parseItem(bytes, pos) }
    val items = itemList.map(_._1)
    Console.err.println("READ " + items)///
    val s1 = itemList.lastOption.map(_._2).getOrElse(s0 + 2)
    val timestamp = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
    val feeAmount = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
    val txEi = for {
      proofs <- Proofs.fromBytes(bytes.drop(s1 + 16))
      tx <- create(version, sender, items, feeAmount, timestamp, proofs)
    } yield tx
    txEi.fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             data: List[ParsedItem],
             feeAmount: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, DataTransaction] = {
    if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(DataTransaction(version, sender, data, feeAmount, timestamp, proofs))
    }
  }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 data: List[ParsedItem],
                 feeAmount: Long,
                 timestamp: Long): Either[ValidationError, DataTransaction] = {
    create(version, sender, data, feeAmount, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(sender, unsigned.bodyBytes())))).explicitGet())
    }
  }
}
