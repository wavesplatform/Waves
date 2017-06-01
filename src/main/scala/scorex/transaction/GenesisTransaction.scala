package scorex.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.state2.{ByteArray, EqByteArray}
import play.api.libs.json.{JsObject, Json}
import scorex.account.Account
import scorex.crypto.hash.FastCryptographicHash._
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

sealed trait GenesisTransaction extends Transaction {
  def recipient: Account
  def amount: Long
  def signature: ByteArray
}

object GenesisTransaction extends {

  private case class GenesisTransactionImpl(recipient: Account, amount: Long, timestamp: Long, signature: ByteArray) extends GenesisTransaction {

    override val assetFee: (Option[AssetId], Long) = (None, 0)
    override val id: EqByteArray                   = signature

    val transactionType = TransactionType.GenesisTransaction

    lazy val creator: Option[Account] = None

    lazy val json: JsObject =
      Json.obj("type"      -> transactionType.id,
               "id"        -> id.base58,
               "fee"       -> 0,
               "timestamp" -> timestamp,
               "signature" -> this.signature.base58,
               "recipient" -> recipient.address,
               "amount"    -> amount)

    lazy val bytes: Array[Byte] = {
      val typeBytes      = Array(transactionType.id.toByte)
      val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
      val amountBytes    = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
      val rcpBytes       = recipient.bytes
      require(rcpBytes.length == Account.AddressLength)
      val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
      require(res.length == TypeLength + BASE_LENGTH)
      res
    }
  }

  private val RECIPIENT_LENGTH = Account.AddressLength
  private val BASE_LENGTH      = TimestampLength + RECIPIENT_LENGTH + AmountLength

  def generateSignature(recipient: Account, amount: Long, timestamp: Long): Array[Byte] = {
    val typeBytes      = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GenesisTransaction.id), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Longs.toByteArray(amount)
    val amountFill     = new Array[Byte](AmountLength - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes, Bytes.concat(amountFill, amountBytes))

    val h = hash(data)
    Bytes.concat(h, h)
  }


  def parseTail(data: Array[Byte]): Try[GenesisTransaction] =
    Try {
      require(data.length >= BASE_LENGTH, "Data does not match base length")

      var position = 0

      val timestampBytes = java.util.Arrays.copyOfRange(data, position, position + TimestampLength)
      val timestamp      = Longs.fromByteArray(timestampBytes)
      position += TimestampLength

      val recipientBytes = java.util.Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH)
      val recipient      = Account.fromBytes(recipientBytes).right.get
      position += RECIPIENT_LENGTH

      val amountBytes = java.util.Arrays.copyOfRange(data, position, position + AmountLength)
      val amount      = Longs.fromByteArray(amountBytes)

      GenesisTransaction.create(recipient, amount, timestamp).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(recipient: Account, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    if (amount < 0) {
      Left(ValidationError.NegativeAmount)
    } else {
      val signature = EqByteArray(GenesisTransaction.generateSignature(recipient, amount, timestamp))
      Right(GenesisTransactionImpl(recipient, amount, timestamp, signature))
    }
  }
}
