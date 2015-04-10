package scorex.transaction

import java.math.BigInteger
import java.util.Arrays

import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json.Json
import scorex.account.Account
import scorex.crypto.{Base58, Crypto}
import scorex.transaction.Transaction.TransactionType


case class GenesisTransaction(override val recipient: Account,
                              override val amount: BigDecimal,
                              override val timestamp: Long)
  extends Transaction(TransactionType.GENESIS_TRANSACTION, recipient, amount, BigDecimal.apply(0), timestamp,
    GenesisTransaction.generateSignature(recipient, amount, timestamp)) {

  import scorex.transaction.GenesisTransaction._
  import scorex.transaction.Transaction._

  override def toJson() =
    getJsonBase() ++ Json.obj("recipient" -> recipient.address, "amount" -> amount.toString())

  override def toBytes() = {
    val typeBytes = Array(TransactionType.GENESIS_TRANSACTION.id.toByte)

    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TIMESTAMP_LENGTH, 0)

    val amountBytes = amount.bigDecimal.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)

    val rcpBytes = Base58.decode(recipient.address)
    require(rcpBytes.length == Account.ADDRESS_LENGTH)

    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, Bytes.concat(amountFill, amountBytes))
    require(res.length == dataLength)
    res
  }

  override lazy val dataLength = TYPE_LENGTH + BASE_LENGTH


  //VALIDATE

  def isSignatureValid() = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GENESIS_TRANSACTION.id), TYPE_LENGTH, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TIMESTAMP_LENGTH, 0)
    val amountBytes = amount.bigDecimal.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)
    val data = Bytes.concat(typeBytes, timestampBytes,
      Base58.decode(recipient.address), Bytes.concat(amountFill, amountBytes))
    val digest = Crypto.sha256(data)

    Bytes.concat(digest, digest).sameElements(signature)
  }

  override def isValid() =
    if (amount < BigDecimal(0)) {
      ValidationResult.NEGATIVE_AMOUNT
    } else if (!Crypto.isValidAddress(recipient.address)) {
      ValidationResult.INVALID_ADDRESS
    } else ValidationResult.VALIDATE_OKE

  override def getCreator(): Option[Account] = None

  override def getInvolvedAccounts() = List(recipient)

  override def isInvolved(account: Account) = recipient.address.equals(account.address)

  override def getAmount(account: Account): BigDecimal =
    if (recipient.address.equals(account.address)) amount else 0
}


object GenesisTransaction {

  import scorex.transaction.Transaction._

  private val RECIPIENT_LENGTH = Account.ADDRESS_LENGTH
  private val BASE_LENGTH = TIMESTAMP_LENGTH + RECIPIENT_LENGTH + AMOUNT_LENGTH

  def generateSignature(recipient: Account, amount: BigDecimal, timestamp: Long) = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GENESIS_TRANSACTION.id), TYPE_LENGTH, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TIMESTAMP_LENGTH, 0)
    val amountBytes = amount.bigDecimal.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes,
      Base58.decode(recipient.address), Bytes.concat(amountFill, amountBytes))

    val digest = Crypto.sha256(data)
    Bytes.concat(digest, digest)
  }

  def Parse(data: Array[Byte]): Transaction = {
    require(data.length >= BASE_LENGTH, "Data does not match block length") //CHECK IF WE MATCH BLOCK LENGTH

    var position = 0

    //READ TIMESTAMP
    val timestampBytes = Arrays.copyOfRange(data, position, position + TIMESTAMP_LENGTH)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TIMESTAMP_LENGTH

    //READ RECIPIENT
    val recipientBytes = Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH)
    val recipient = new Account(Base58.encode(recipientBytes))
    position += RECIPIENT_LENGTH

    //READ AMOUNT
    val amountBytes = Arrays.copyOfRange(data, position, position + AMOUNT_LENGTH)
    val amount = BigDecimal(new BigInteger(amountBytes), 8)

    GenesisTransaction(recipient, amount, timestamp)
  }
}
