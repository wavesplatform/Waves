package scorex.transaction

import java.math.BigDecimal
import java.math.BigInteger
import java.util.Arrays
import play.api.libs.json.Json

import scorex.account.Account
import scorex.crypto.Base58
import scorex.crypto.Crypto

import com.google.common.primitives.Bytes
import com.google.common.primitives.Ints
import com.google.common.primitives.Longs

import database.DBSet
import scorex.transaction.Transaction.TransactionType

class GenesisTransaction(recipient: Account, amount: BigDecimal, timestamp: Long)
  extends Transaction(TransactionType.GENESIS_TRANSACTION, BigDecimal.ZERO, timestamp, new Array[Byte](0),
    GenesisTransaction.generateSignature(recipient, amount, timestamp)) {

  import Transaction._
  import GenesisTransaction._

  override def toJson() =
    getJsonBase() ++ Json.obj("recipient" -> recipient.getAddress(), "amount" -> amount.toPlainString())

  override def toBytes() = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GENESIS_TRANSACTION.id), TYPE_LENGTH, 0)

    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TIMESTAMP_LENGTH, 0)

    val amountBytes = amount.unscaledValue().toByteArray()
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)
    Bytes.concat(typeBytes, timestampBytes, amountBytes,
      Base58.decode(recipient.getAddress()), Bytes.concat(amountFill, amountBytes))
  }

  override def getDataLength() = TYPE_LENGTH + BASE_LENGTH


  //VALIDATE

  def isSignatureValid() = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GENESIS_TRANSACTION.id), TYPE_LENGTH, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TIMESTAMP_LENGTH, 0)
    val amountBytes = amount.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)
    val data = Bytes.concat(typeBytes, timestampBytes,
      Base58.decode(recipient.getAddress), Bytes.concat(amountFill, amountBytes))
    val digest = Crypto.sha256(data)

    Bytes.concat(digest, digest).sameElements(signature)
  }

  override def isValid(db: DBSet) =
    if (amount.compareTo(BigDecimal.ZERO) == -1) {
      ValidationResult.NEGATIVE_AMOUNT
    } else if (!Crypto.isValidAddress(recipient.getAddress)) {
      ValidationResult.INVALID_ADDRESS
    } else ValidationResult.VALIDATE_OKE


  //PROCESS/ORPHAN

  override def process(db: DBSet) {
    recipient.setConfirmedBalance(amount, db) //UPDATE BALANCE
    recipient.setLastReference(signature, db) //SET AS REFERENCE
  }

  override def orphan(db: DBSet) = {
    recipient.setConfirmedBalance(BigDecimal.ZERO, db) //UNDO BALANCE
    recipient.removeReference(db) //UNDO REFERENCE
  }

  override def getCreator(): Account = null //todo: Option

  override def getInvolvedAccounts() = List(recipient)

  override def isInvolved(account: Account) = recipient.getAddress.equals(account.getAddress)

  override def getAmount(account: Account) = {
    if (recipient.getAddress.equals(account.getAddress)) {
      amount
    } else BigDecimal.ZERO
  }
}


object GenesisTransaction {

  import Transaction._

  private val RECIPIENT_LENGTH = Account.ADDRESS_LENGTH
  private val AMOUNT_LENGTH = 8
  private val BASE_LENGTH = TIMESTAMP_LENGTH + RECIPIENT_LENGTH + AMOUNT_LENGTH

  def generateSignature(recipient: Account, amount: BigDecimal, timestamp: Long) = {
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.GENESIS_TRANSACTION.id), TYPE_LENGTH, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TIMESTAMP_LENGTH, 0)
    val amountBytes = amount.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes,
      Base58.decode(recipient.getAddress), Bytes.concat(amountFill, amountBytes))

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
    val amount = new BigDecimal(new BigInteger(amountBytes), 8)

    new GenesisTransaction(recipient, amount, timestamp)
  }
}
