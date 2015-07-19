package scorex.transaction

import java.util.Arrays

import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.controller.Controller
import play.api.libs.json.Json
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.{Base58, Crypto}
import scorex.transaction.Transaction.TransactionType

case class PaymentTransaction(sender: PublicKeyAccount,
                              override val recipient: Account,
                              override val amount: Long,
                              override val fee: Long,
                              override val timestamp: Long,
                              override val signature: Array[Byte])
  extends Transaction(TransactionType.PaymentTransaction, recipient, amount, fee, timestamp, signature) {

  import scorex.transaction.PaymentTransaction._
  import scorex.transaction.Transaction._

  override lazy val dataLength = TypeLength + BASE_LENGTH

  override def json() = jsonBase() ++ Json.obj(
    "sender" -> sender.address,
    "recipient" -> recipient.address,
    "amount" -> amount
  )

  override def bytes() = {
    //WRITE TYPE
    val typeBytes = Array(TypeId.toByte)

    //WRITE TIMESTAMP
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)

    //WRITE AMOUNT
    val amountBytes = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)

    //WRITE FEE
    val feeBytes = Bytes.ensureCapacity(Longs.toByteArray(fee), FEE_LENGTH, 0)

    Bytes.concat(typeBytes, timestampBytes, sender.publicKey,
      Base58.decode(recipient.address).get, amountBytes,
      feeBytes, signature)
  }

  override def isSignatureValid() = {
    val data = signatureData(sender, recipient, amount, fee, timestamp)
    Crypto.verify(signature, data, sender.publicKey)
  }

  override def validate() =
    if (!Crypto.isValidAddress(recipient.address)) {
      ValidationResult.InvalidAddress //CHECK IF RECIPIENT IS VALID ADDRESS
    } else if (Controller.blockchainStorage.balance(sender.address) < amount + fee) {
      ValidationResult.NoBalance //CHECK IF SENDER HAS ENOUGH MONEY
    } else if (amount <= BigDecimal(0)) {
      ValidationResult.NegativeAmount //CHECK IF AMOUNT IS POSITIVE
    } else if (fee <= BigDecimal(0)) {
      ValidationResult.NegativeFee //CHECK IF FEE IS POSITIVE
    } else ValidationResult.ValidateOke

  override def getCreator() = Some(sender)

  override def involvedAmount(account: Account) = {
    val address = account.address

    if (address.equals(sender.address) && address.equals(recipient.address)) {
      BigDecimal(0).setScale(8) - fee
    } else if (address.equals(sender.address)) {
      BigDecimal(0).setScale(8) - amount - fee
    } else if (address.equals(recipient.address)) {
      amount
    } else BigDecimal(0)
  }

  override def balanceChanges(): Map[Option[Account], BigDecimal] =
    Map(Some(sender) -> -amount, Some(recipient) -> amount, None -> fee)
}


object PaymentTransaction {

  import scorex.transaction.Transaction._

  private val SENDER_LENGTH = 32
  private val FEE_LENGTH = 8
  private val SIGNATURE_LENGTH = 64
  private val BASE_LENGTH = TimestampLength + SENDER_LENGTH + RecipientLength + AmountLength + FEE_LENGTH + SIGNATURE_LENGTH

  def apply(sender: PrivateKeyAccount, recipient: Account,
            amount: Long, fee: Long, timestamp: Long): PaymentTransaction = {
    val sig = generateSignature(sender, recipient, amount, fee, timestamp)
    PaymentTransaction(sender, recipient, amount, fee, timestamp, sig)
  }

  def parse(data: Array[Byte]) = {
    require(data.length >= BASE_LENGTH, "Data does not match base length")

    var position = 0

    //READ TIMESTAMP
    val timestampBytes = data.take(TimestampLength)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TimestampLength

    //READ SENDER
    val senderBytes = Arrays.copyOfRange(data, position, position + SENDER_LENGTH)
    val sender = new PublicKeyAccount(senderBytes)
    position += SENDER_LENGTH

    //READ RECIPIENT
    val recipientBytes = Arrays.copyOfRange(data, position, position + RecipientLength)
    val recipient = new Account(Base58.encode(recipientBytes))
    position += RecipientLength

    //READ AMOUNT
    val amountBytes = Arrays.copyOfRange(data, position, position + AmountLength)
    val amount = Longs.fromByteArray(amountBytes)
    position += AmountLength

    //READ FEE
    val feeBytes = Arrays.copyOfRange(data, position, position + FEE_LENGTH)
    val fee = Longs.fromByteArray(feeBytes)
    position += FEE_LENGTH

    //READ SIGNATURE
    val signatureBytes = Arrays.copyOfRange(data, position, position + SIGNATURE_LENGTH)

    new PaymentTransaction(sender, recipient, amount, fee, timestamp, signatureBytes)
  }

  def generateSignature(sender: PrivateKeyAccount, recipient: Account,
                        amount: Long, fee: Long, timestamp: Long): Array[Byte] = {
    Crypto.sign(sender, signatureData(sender, recipient, amount, fee, timestamp))
  }

  private def signatureData(sender: PublicKeyAccount, recipient: Account,
                            amount: Long, fee: Long, timestamp: Long): Array[Byte] = {
    //WRITE TYPE
    val typeBytes = Bytes.ensureCapacity(Ints.toByteArray(TransactionType.PaymentTransaction.id), TypeLength, 0)

    //WRITE TIMESTAMP
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)

    //WRITE AMOUNT
    val amountBytes = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)

    //WRITE FEE
    val feeBytes = Bytes.ensureCapacity(Longs.toByteArray(fee), FEE_LENGTH, 0)

    Bytes.concat(typeBytes, timestampBytes, sender.publicKey,
      Base58.decode(recipient.address).get, amountBytes, feeBytes)
  }
}