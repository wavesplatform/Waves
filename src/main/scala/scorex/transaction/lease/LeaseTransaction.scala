package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.TypedTransaction._
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait LeaseTransaction extends SignedTransaction {
  def amount: Long

  def fee: Long

  def recipient: Account
}

object LeaseTransaction extends Deser[LeaseTransaction] {

  private case class LeaseTransactionImpl(sender: PublicKeyAccount,
                                          amount: Long,
                                          fee: Long,
                                          timestamp: Long,
                                          recipient: Account,
                                          signature: Array[Byte])
    extends LeaseTransaction {

    override val transactionType: TransactionType.Value = TransactionType.LeaseTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
      sender.publicKey,
      recipient.bytes,
      Longs.toByteArray(amount),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "amount" -> amount,
      "recipient" -> recipient.address,
      "fee" -> fee,
      "timestamp" -> timestamp
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)
    override lazy val balanceChanges: Seq[BalanceChange] = Seq(BalanceChange(AssetAcc(sender, None), -fee))
    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  }

  override def parseBytes(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    require(bytes.head == TransactionType.LeaseTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    import EllipticCurveImpl._
    val sender = new PublicKeyAccount(bytes.slice(0, KeyLength))
    val recipient = new Account(Base58.encode(bytes.slice(KeyLength, KeyLength + Account.AddressLength)))
    val quantityStart = KeyLength + Account.AddressLength

    val quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val fee = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
    val signature = bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength)
    LeaseTransaction
      .create(sender, quantity, fee, timestamp, recipient, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               amount: Long,
                               fee: Long,
                               timestamp: Long,
                               recipient: Account,
                               signature: Option[Array[Byte]] = None): Either[ValidationError, LeaseTransactionImpl] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (sender.address == recipient.address) {
      Left(ValidationError.ToSelf)
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else if (!Account.isValid(recipient)) {
      Left(ValidationError.InvalidAddress)
    } else if (!Account.isValid(sender)) {
      Left(ValidationError.InvalidAddress)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(LeaseTransactionImpl(sender, amount, fee, timestamp, recipient, signature.orNull))
    }
  }

  def create(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: Account,
             signature: Array[Byte]): Either[ValidationError, LeaseTransaction] = {
    createUnverified(sender, amount, fee, timestamp, recipient, Some(signature))
      .right.flatMap(SignedTransaction.verify)
  }

  def create(sender: PrivateKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: Account): Either[ValidationError, LeaseTransaction] = {
    createUnverified(sender, amount, fee, timestamp, recipient).right.map { unsigned =>
      unsigned.copy(signature = EllipticCurveImpl.sign(sender, unsigned.toSign))
    }
  }
}
