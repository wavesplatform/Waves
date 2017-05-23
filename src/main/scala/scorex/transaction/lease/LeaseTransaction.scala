package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, AccountOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser._
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait LeaseTransaction extends SignedTransaction {
  def amount: Long

  def fee: Long

  def recipient: AccountOrAlias
}

object LeaseTransaction {

  private case class LeaseTransactionImpl(sender: PublicKeyAccount,
                                          amount: Long,
                                          fee: Long,
                                          timestamp: Long,
                                          recipient: AccountOrAlias,
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
      "recipient" -> recipient.stringRepr,
      "fee" -> fee,
      "timestamp" -> timestamp
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)
    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  }

  def parseTail(bytes: Array[Byte]): Try[LeaseTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    (for {
      recRes <- AccountOrAlias.fromBytes(bytes, KeyLength)
      (recipient, recipientEnd) = recRes
      quantityStart = recipientEnd
      quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
      fee = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
      timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
      signature = bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength)
      lt <- LeaseTransaction.create(sender, quantity, fee, timestamp, recipient, signature)
    } yield lt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               amount: Long,
                               fee: Long,
                               timestamp: Long,
                               recipient: AccountOrAlias,
                               signature: Option[Array[Byte]] = None): Either[ValidationError, LeaseTransactionImpl] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount)

    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (recipient.isInstanceOf[Account] && sender.stringRepr == recipient.stringRepr) {
      Left(ValidationError.ToSelf)
    } else {
      Right(LeaseTransactionImpl(sender, amount, fee, timestamp, recipient, signature.orNull))
    }
  }

  def create(sender: PublicKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AccountOrAlias,
             signature: Array[Byte]): Either[ValidationError, LeaseTransaction] = {
    createUnverified(sender, amount, fee, timestamp, recipient, Some(signature))
      .right.flatMap(SignedTransaction.verify)
  }

  def create(sender: PrivateKeyAccount,
             amount: Long,
             fee: Long,
             timestamp: Long,
             recipient: AccountOrAlias): Either[ValidationError, LeaseTransaction] = {
    createUnverified(sender, amount, fee, timestamp, recipient).right.map { unsigned =>
      unsigned.copy(signature = EllipticCurveImpl.sign(sender, unsigned.toSign))
    }
  }
}
