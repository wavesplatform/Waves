package scorex.transaction

import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.Account
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash._
import scorex.serialization.Deser
import scorex.transaction.LagonakiTransaction.TransactionType

import scala.util.{Failure, Try}


case class GenesisTransaction(override val recipient: Account,
                              override val amount: Long,
                              override val timestamp: Long)
  extends LagonakiTransaction(TransactionType.GenesisTransaction, recipient, amount, 0, timestamp,
    GenesisTransaction.generateSignature(recipient, amount, timestamp)) {

  import scorex.transaction.GenesisTransaction._
  import scorex.transaction.LagonakiTransaction._

  override lazy val creator: Option[Account] = None

  override lazy val json: JsObject =
    jsonBase() ++ Json.obj("recipient" -> recipient.address, "amount" -> amount)

  override lazy val bytes: Array[Byte] = {
    val typeBytes = Array(TransactionType.GenesisTransaction.id.toByte)

    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)

    val amountBytes = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)

    val rcpBytes = recipient.bytes
    require(rcpBytes.length == Account.AddressLength)

    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
    require(res.length == dataLength)
    res
  }

  override lazy val dataLength = TypeLength + BASE_LENGTH

  override lazy val signatureValid: Boolean = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GenesisTransaction.id), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes, amountBytes)

    val h = hash(data)
    Bytes.concat(h, h).sameElements(signature)
  }

  override def validate: ValidationResult.Value =
    if (amount < 0) {
      ValidationResult.NegativeAmount
    } else if (!Account.isValid(recipient)) {
      ValidationResult.InvalidAddress
    } else ValidationResult.ValidateOke

  override def involvedAmount(account: Account): Long = if (recipient.address.equals(account.address)) amount else 0

  override def balanceChanges(): Seq[(Account, Long)] = Seq((recipient, amount))
}


object GenesisTransaction extends Deser[GenesisTransaction] {

  import scorex.transaction.LagonakiTransaction._

  private val RECIPIENT_LENGTH = Account.AddressLength
  private val BASE_LENGTH = TimestampLength + RECIPIENT_LENGTH + AmountLength

  def generateSignature(recipient: Account, amount: Long, timestamp: Long): Array[Byte] = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GenesisTransaction.id), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes = Longs.toByteArray(amount)
    val amountFill = new Array[Byte](AmountLength - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes, Bytes.concat(amountFill, amountBytes))

    val h = hash(data)
    Bytes.concat(h, h)
  }

  def parseBytes(data: Array[Byte]): Try[GenesisTransaction] = {
    data.head match {
      case transactionType: Byte if transactionType == TransactionType.GenesisTransaction.id =>
        parseTail(data.tail)
      case transactionType =>
        Failure(new Exception(s"Incorrect transaction type '$transactionType' in GenesisTransaction data"))
    }
  }

  def parseTail(data: Array[Byte]): Try[GenesisTransaction] = Try {
    require(data.length >= BASE_LENGTH, "Data does not match base length")

    var position = 0

    val timestampBytes = java.util.Arrays.copyOfRange(data, position, position + TimestampLength)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TimestampLength

    val recipientBytes = java.util.Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH)
    val recipient = new Account(Base58.encode(recipientBytes))
    position += RECIPIENT_LENGTH

    val amountBytes = java.util.Arrays.copyOfRange(data, position, position + AmountLength)
    val amount = Longs.fromByteArray(amountBytes)

    GenesisTransaction(recipient, amount, timestamp)
  }
}
