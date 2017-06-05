package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.EllipticCurveImpl.SignatureLength
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.transaction.TransactionParser.{KeyLength, _}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait LeaseCancelTransaction extends SignedTransaction {
  def fee: Long

  def leaseId: ByteStr
}

object LeaseCancelTransaction {

  private case class LeaseCancelTransactionImpl(sender: PublicKeyAccount,
                                                leaseId: ByteStr,
                                                fee: Long,
                                                timestamp: Long,
                                                signature: ByteStr)
    extends LeaseCancelTransaction {

    override val transactionType: TransactionType.Value = TransactionType.LeaseCancelTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
      sender.publicKey,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp),
      leaseId.arr)

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "fee" -> fee,
      "timestamp" -> timestamp,
      "leaseId" -> leaseId.base58
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)
    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

  }

  def parseTail(bytes: Array[Byte]): Try[LeaseCancelTransaction] = Try {
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val fee = Longs.fromByteArray(bytes.slice(KeyLength, KeyLength + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(KeyLength + 8, KeyLength + 16))
    val leaseId = ByteStr(bytes.slice(KeyLength + 16, KeyLength + 16 + DigestSize))
    val signature =ByteStr(bytes.slice(KeyLength + 16 + DigestSize, KeyLength + 16 + DigestSize + SignatureLength))
    LeaseCancelTransaction
      .create(sender, leaseId, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               leaseId: ByteStr,
                               fee: Long,
                               timestamp: Long,
                               signature: Option[ByteStr] = None): Either[ValidationError, LeaseCancelTransactionImpl] = {
    if (leaseId.arr.length != DigestSize) {
      Left(ValidationError.TransactionParameterValidationError("Lease transaction id is invalid"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(LeaseCancelTransactionImpl(sender, leaseId, fee, timestamp, signature.orNull))
    }
  }

  def create(sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, LeaseCancelTransaction] = {
    createUnverified(sender, leaseId, fee, timestamp, Some(signature))
      .right.flatMap(SignedTransaction.verify)
  }

  def create(sender: PrivateKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long): Either[ValidationError, LeaseCancelTransaction] = {
    createUnverified(sender, leaseId, fee, timestamp).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
