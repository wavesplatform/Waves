package scorex.transaction

import java.math.BigDecimal
import java.math.BigInteger
import java.util.Arrays

import play.api.libs.json.Json

import scorex.account.{PublicKeyAccount, Account, PrivateKeyAccount}

import scorex.crypto.Base58
import scorex.crypto.Crypto

import com.google.common.primitives.Bytes
import com.google.common.primitives.Ints
import com.google.common.primitives.Longs

import database.DBSet
import scorex.transaction.Transaction.TransactionType

case class PaymentTransaction(sender: PublicKeyAccount,
                              recipient: Account,
                              amount: BigDecimal,
                              override val fee: BigDecimal,
                              override val timestamp: Long,
                              override val reference: Array[Byte],
                              override val signature: Array[Byte])
  extends Transaction(TransactionType.PAYMENT_TRANSACTION, fee, timestamp, reference, signature) {

  import PaymentTransaction._
  import Transaction._

  override def toJson() = getJsonBase() ++ Json.obj(
    "sender" -> sender.getAddress,
    "recipient" -> recipient.getAddress,
    "amount" -> amount.toPlainString
  )

  override def toBytes() = {
    //WRITE TYPE
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TypeId), TYPE_LENGTH, 0)

    //WRITE TIMESTAMP
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(this.timestamp), TIMESTAMP_LENGTH, 0)

    //WRITE AMOUNT
    val amountBytes = this.amount.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)

    //WRITE FEE
    val feeBytes = this.fee.unscaledValue().toByteArray
    val feeFill = new Array[Byte](FEE_LENGTH - feeBytes.length)

    Bytes.concat(typeBytes, timestampBytes, reference, sender.getPublicKey,
      Base58.decode(recipient.getAddress), Bytes.concat(amountFill, amountBytes),
      Bytes.concat(feeFill, feeBytes, signature)
    )
  }

  override def getDataLength() = TYPE_LENGTH + BASE_LENGTH

  //VALIDATE

  override def isSignatureValid() = {
    //WRITE TYPE
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TypeId), TYPE_LENGTH, 0)

    //WRITE TIMESTAMP
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(this.timestamp), TIMESTAMP_LENGTH, 0)

    //WRITE AMOUNT
    val amountBytes = this.amount.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)

    //WRITE FEE
    val feeBytes = this.fee.unscaledValue().toByteArray
    val feeFill = new Array[Byte](FEE_LENGTH - feeBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes, this.reference, this.sender.getPublicKey,
      Base58.decode(this.recipient.getAddress), Bytes.concat(amountFill, amountBytes), Bytes.concat(feeFill, feeBytes))

    Crypto.verify(this.sender.getPublicKey, this.signature, data)
  }

  override def isValid(db: DBSet) =
    if (!Crypto.isValidAddress(recipient.getAddress)) {
      ValidationResult.INVALID_ADDRESS //CHECK IF RECIPIENT IS VALID ADDRESS
    } else if (this.sender.getBalance(1, db).compareTo(this.amount.add(this.fee)) == -1) {
      ValidationResult.NO_BALANCE //CHECK IF SENDER HAS ENOUGH MONEY
    } else if (!Arrays.equals(this.sender.getLastReference(db), this.reference)) {
      ValidationResult.INVALID_REFERENCE //CHECK IF REFERENCE IS OKE
      //todo: causes network stuck if one account sumbits two txs to be included into one block with older tx fee
      //todo: more than newer's one
    } else if (this.amount.compareTo(BigDecimal.ZERO) <= 0) {
      ValidationResult.NEGATIVE_AMOUNT //CHECK IF AMOUNT IS POSITIVE
    } else if (this.fee.compareTo(BigDecimal.ZERO) <= 0) {
      ValidationResult.NEGATIVE_FEE //CHECK IF FEE IS POSITIVE
    } else ValidationResult.VALIDATE_OKE


  //PROCESS/ORPHAN

  override def process(db: DBSet) {
    //UPDATE SENDER
    this.sender.setConfirmedBalance(this.sender.getConfirmedBalance(db).subtract(this.amount).subtract(this.fee), db)

    //UPDATE RECIPIENT
    this.recipient.setConfirmedBalance(this.recipient.getConfirmedBalance(db).add(this.amount), db)

    //UPDATE REFERENCE OF SENDER
    this.sender.setLastReference(this.signature, db)

    //UPDATE REFERENCE OF RECIPIENT
    if (Arrays.equals(this.recipient.getLastReference(db), new Array[Byte](0))) {
      this.recipient.setLastReference(this.signature, db)
    }
  }

  override def orphan(db: DBSet) {
    //UPDATE SENDER
    this.sender.setConfirmedBalance(this.sender.getConfirmedBalance(db).add(this.amount).add(this.fee), db)

    //UPDATE RECIPIENT
    this.recipient.setConfirmedBalance(this.recipient.getConfirmedBalance(db).subtract(this.amount), db)

    //UPDATE REFERENCE OF SENDER
    this.sender.setLastReference(this.reference, db)

    ///UPDATE REFERENCE OF RECIPIENT
    if (Arrays.equals(this.recipient.getLastReference(db), this.signature)) {
      this.recipient.removeReference(db)
    }
  }

  override def getCreator() = this.sender

  override def getInvolvedAccounts() = List(sender, recipient)

  override def isInvolved(account: Account) = {
    val address = account.getAddress
    address.equals(sender.getAddress) || address.equals(recipient.getAddress)
  }

  override def getAmount(account: Account) = {
    val address = account.getAddress

    //CHECK OF BOTH SENDER AND RECIPIENT
    if (address.equals(sender.getAddress) && address.equals(recipient.getAddress)) {
      BigDecimal.ZERO.setScale(8).subtract(fee)
    } else if (address.equals(sender.getAddress)) {
      BigDecimal.ZERO.setScale(8).subtract(amount).subtract(fee)
    } else if (address.equals(recipient.getAddress)) {
      amount
    } else {
      BigDecimal.ZERO
    }
  }
}


object PaymentTransaction {

  import Transaction._

  private val REFERENCE_LENGTH = 64
  private val SENDER_LENGTH = 32
  private val RECIPIENT_LENGTH = Account.ADDRESS_LENGTH
  private val AMOUNT_LENGTH = 8
  private val FEE_LENGTH = 8
  private val SIGNATURE_LENGTH = 64
  private val BASE_LENGTH = TIMESTAMP_LENGTH + REFERENCE_LENGTH + SENDER_LENGTH + RECIPIENT_LENGTH + AMOUNT_LENGTH + FEE_LENGTH + SIGNATURE_LENGTH


  def Parse(data: Array[Byte]) = {
    require(data.length >= BASE_LENGTH, "Data does not match base length")

    var position = 0

    //READ TIMESTAMP
    val timestampBytes = Arrays.copyOfRange(data, position, position + TIMESTAMP_LENGTH)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TIMESTAMP_LENGTH

    //READ REFERENCE
    val reference = Arrays.copyOfRange(data, position, position + REFERENCE_LENGTH)
    position += REFERENCE_LENGTH

    //READ SENDER
    val senderBytes = Arrays.copyOfRange(data, position, position + SENDER_LENGTH)
    val sender = new PublicKeyAccount(senderBytes)
    position += SENDER_LENGTH

    //READ RECIPIENT
    val recipientBytes = Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH)
    val recipient = new Account(Base58.encode(recipientBytes))
    position += RECIPIENT_LENGTH

    //READ AMOUNT
    val amountBytes = Arrays.copyOfRange(data, position, position + AMOUNT_LENGTH)
    val amount = new BigDecimal(new BigInteger(amountBytes), 8)
    position += AMOUNT_LENGTH

    //READ FEE
    val feeBytes = Arrays.copyOfRange(data, position, position + FEE_LENGTH)
    val fee = new BigDecimal(new BigInteger(feeBytes), 8)
    position += FEE_LENGTH

    //READ SIGNATURE
    val signatureBytes = Arrays.copyOfRange(data, position, position + SIGNATURE_LENGTH)

    new PaymentTransaction(sender, recipient, amount, fee, timestamp, reference, signatureBytes)
  }

  def generateSignature(sender: PrivateKeyAccount, recipient: Account, amount: BigDecimal,
                        fee: BigDecimal, timestamp: Long): Array[Byte]
  = generateSignature(DBSet.getInstance(), sender, recipient, amount, fee, timestamp)


  def generateSignature(db: DBSet, sender: PrivateKeyAccount, recipient: Account,
                        amount: BigDecimal, fee: BigDecimal, timestamp: Long): Array[Byte] = {
    //WRITE TYPE
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.PAYMENT_TRANSACTION.id), TYPE_LENGTH, 0)

    //WRITE TIMESTAMP
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TIMESTAMP_LENGTH, 0)

    //WRITE AMOUNT
    val amountBytes = amount.unscaledValue().toByteArray
    val amountFill = new Array[Byte](AMOUNT_LENGTH - amountBytes.length)

    //WRITE FEE
    val feeBytes = fee.unscaledValue().toByteArray
    val feeFill = new Array[Byte](FEE_LENGTH - feeBytes.length)

    val data = Bytes.concat(typeBytes,
      timestampBytes,
      sender.getLastReference(db),
      sender.getPublicKey,
      Base58.decode(recipient.getAddress), //todo: possible exception here
      Bytes.concat(amountFill, amountBytes),
      Bytes.concat(feeFill, feeBytes))

    Crypto.sign(sender, data)
  }
}