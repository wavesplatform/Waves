package scorex.transaction

import java.util

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount._
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.PaymentTransaction.signatureData
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

case class PaymentTransaction private(sender: PublicKeyAccount,
                                      recipient: Address,
                                      amount: Long,
                                      fee: Long,
                                      timestamp: Long,
                                      signature: ByteStr) extends Transaction {
  override val transactionType = TransactionType.PaymentTransaction
  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val id = Coeval.evalOnce(signature)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    Json.obj("type" -> transactionType.id,
      "id" -> id().base58,
      "fee" -> fee,
      "timestamp" -> timestamp,
      "signature" -> this.signature.base58,
      "sender" -> sender.address,
      "senderPublicKey" -> Base58.encode(sender.publicKey),
      "recipient" -> recipient.address,
      "amount" -> amount))

  private val hashBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val feeBytes = Longs.toByteArray(fee)
    Bytes.concat(Array(transactionType.id.toByte), timestampBytes, sender.publicKey, recipient.bytes.arr, amountBytes, feeBytes)
  }

  val hash: Coeval[FastCryptographicHash.Digest] = Coeval.evalOnce(FastCryptographicHash(hashBytes()))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(hashBytes(), signature.arr))

  val signatureValid: Coeval[Boolean] = Coeval.evalOnce(EllipticCurveImpl.verify(signature.arr,
    signatureData(sender, recipient, amount, fee, timestamp), sender.publicKey))
}

object PaymentTransaction {


  val RecipientLength = Address.AddressLength

  private val SenderLength = 32
  private val FeeLength = 8
  private val BaseLength = TimestampLength + SenderLength + RecipientLength + AmountLength + FeeLength + SignatureLength

  def create(sender: PrivateKeyAccount, recipient: Address, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] = {
    create(sender, recipient, amount, fee, timestamp, ByteStr.empty).right.map(unsigned => {
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, signatureData(sender, recipient, amount, fee, timestamp))))
    })
  }

  def create(sender: PublicKeyAccount,
             recipient: Address,
             amount: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, PaymentTransaction] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee) //CHECK IF FEE IS POSITIVE
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else {
      Right(PaymentTransaction(sender, recipient, amount, fee, timestamp, signature))
    }
  }

  def parseTail(data: Array[Byte]): Try[PaymentTransaction] = Try {
    require(data.length >= BaseLength, "Data does not match base length")

    var position = 0

    //READ TIMESTAMP
    val timestampBytes = data.take(TimestampLength)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TimestampLength

    //READ SENDER
    val senderBytes = util.Arrays.copyOfRange(data, position, position + SenderLength)
    val sender = PublicKeyAccount(senderBytes)
    position += SenderLength

    //READ RECIPIENT
    val recipientBytes = util.Arrays.copyOfRange(data, position, position + RecipientLength)
    val recipient = Address.fromBytes(recipientBytes).right.get
    position += RecipientLength

    //READ AMOUNT
    val amountBytes = util.Arrays.copyOfRange(data, position, position + AmountLength)
    val amount = Longs.fromByteArray(amountBytes)
    position += AmountLength

    //READ FEE
    val feeBytes = util.Arrays.copyOfRange(data, position, position + FeeLength)
    val fee = Longs.fromByteArray(feeBytes)
    position += FeeLength

    //READ SIGNATURE
    val signatureBytes = util.Arrays.copyOfRange(data, position, position + SignatureLength)

    PaymentTransaction
      .create(sender, recipient, amount, fee, timestamp, ByteStr(signatureBytes))
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def signatureData(sender: PublicKeyAccount, recipient: Address, amount: Long, fee: Long, timestamp: Long): Array[Byte] = {
    val typeBytes = Ints.toByteArray(TransactionType.PaymentTransaction.id)
    val timestampBytes = Longs.toByteArray(timestamp)
    val amountBytes = Longs.toByteArray(amount)
    val feeBytes = Longs.toByteArray(fee)

    Bytes.concat(typeBytes, timestampBytes, sender.publicKey, recipient.bytes.arr, amountBytes, feeBytes)
  }
}
