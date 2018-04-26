package scorex.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.Address
import scorex.transaction.TransactionParsers._
import scorex.transaction.validation.ValidationError

import scala.util.{Failure, Success, Try}

case class GenesisTransaction private (recipient: Address, amount: Long, timestamp: Long, signature: ByteStr) extends Transaction {

  import GenesisTransaction._

  override val builder: TransactionParser        = GenesisTransaction
  override val assetFee: (Option[AssetId], Long) = (None, 0)
  override val id: Coeval[AssetId]               = Coeval.evalOnce(signature)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    Json.obj(
      "type"      -> builder.typeId,
      "id"        -> id().base58,
      "fee"       -> 0,
      "timestamp" -> timestamp,
      "signature" -> this.signature.base58,
      "recipient" -> recipient.address,
      "amount"    -> amount
    ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val typeBytes      = Array(builder.typeId)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
    val rcpBytes       = recipient.bytes.arr
    require(rcpBytes.length == Address.AddressLength)
    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
    require(res.length == TypeLength + BASE_LENGTH)
    res
  }
}

object GenesisTransaction extends TransactionParserFor[GenesisTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 1

  private val RECIPIENT_LENGTH = Address.AddressLength
  private val BASE_LENGTH      = TimestampLength + RECIPIENT_LENGTH + AmountLength

  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {
    val typeBytes      = Bytes.ensureCapacity(Ints.toByteArray(typeId), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Longs.toByteArray(amount)
    val amountFill     = new Array[Byte](AmountLength - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes.arr, Bytes.concat(amountFill, amountBytes))

    val h = crypto.fastHash(data)
    Bytes.concat(h, h)
  }

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      require(bytes.length >= BASE_LENGTH, "Data does not match base length")

      var position = 0

      val timestampBytes = java.util.Arrays.copyOfRange(bytes, position, position + TimestampLength)
      val timestamp      = Longs.fromByteArray(timestampBytes)
      position += TimestampLength

      val recipientBytes = java.util.Arrays.copyOfRange(bytes, position, position + RECIPIENT_LENGTH)
      val recipient      = Address.fromBytes(recipientBytes).right.get
      position += RECIPIENT_LENGTH

      val amountBytes = java.util.Arrays.copyOfRange(bytes, position, position + AmountLength)
      val amount      = Longs.fromByteArray(amountBytes)

      GenesisTransaction.create(recipient, amount, timestamp).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    if (amount < 0) {
      Left(ValidationError.NegativeAmount(amount, "waves"))
    } else {
      val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))
      Right(GenesisTransaction(recipient, amount, timestamp, signature))
    }
  }
}
