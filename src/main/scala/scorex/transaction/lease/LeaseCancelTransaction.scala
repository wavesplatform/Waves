package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.EllipticCurveImpl.SignatureLength
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.serialization.Deser
import scorex.transaction.TypedTransaction.{KeyLength, _}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait LeaseCancelTransaction extends SignedTransaction {
  def fee: Long

  def leaseId: Array[Byte]
}

object LeaseCancelTransaction extends Deser[LeaseCancelTransaction] {

  private case class LeaseCancelTransactionImpl(sender: PublicKeyAccount,
                                                leaseId: Array[Byte],
                                                fee: Long,
                                                timestamp: Long,
                                                signature: Array[Byte])
    extends LeaseCancelTransaction {

    override val transactionType: TransactionType.Value = TransactionType.LeaseCancelTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
      sender.publicKey,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp),
      leaseId)

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "fee" -> fee,
      "timestamp" -> timestamp,
      "leaseId" -> Base58.encode(leaseId)
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)
    override lazy val balanceChanges: Seq[BalanceChange] = Seq(BalanceChange(AssetAcc(sender, None), -fee))

    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  }

  override def parseBytes(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    require(bytes.head == TransactionType.LeaseCancelTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    val sender = new PublicKeyAccount(bytes.slice(0, KeyLength))
    val fee = Longs.fromByteArray(bytes.slice(KeyLength, KeyLength + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(KeyLength + 8, KeyLength + 16))
    val leaseId = bytes.slice(KeyLength + 16, KeyLength + 16 + DigestSize)
    val signature = bytes.slice(KeyLength + 16 + DigestSize, KeyLength + 16 + DigestSize + SignatureLength)
    LeaseCancelTransaction
      .create(sender, leaseId, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               leaseId: Array[Byte],
                               fee: Long,
                               timestamp: Long,
                               signature: Option[Array[Byte]] = None): Either[ValidationError, LeaseCancelTransactionImpl] = {
    if (leaseId.length != DigestSize) {
      Left(ValidationError.NegativeAmount)
    } else if (!Account.isValid(sender)) {
      Left(ValidationError.InvalidAddress)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(LeaseCancelTransactionImpl(sender, leaseId, fee, timestamp, signature.orNull))
    }
  }

  def create(sender: PublicKeyAccount,
             leaseId: Array[Byte],
             fee: Long,
             timestamp: Long,
             signature: Array[Byte]): Either[ValidationError, LeaseCancelTransaction] = {
    createUnverified(sender, leaseId, fee, timestamp, Some(signature))
      .right.flatMap(SignedTransaction.verify)
  }

  def create(sender: PrivateKeyAccount,
             leaseId: Array[Byte],
             fee: Long,
             timestamp: Long): Either[ValidationError, LeaseCancelTransaction] = {
    createUnverified(sender, leaseId, fee, timestamp).right.map { unsigned =>
      unsigned.copy(signature = EllipticCurveImpl.sign(sender, unsigned.toSign))
    }
  }
}
