package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.TypedTransaction._
import scorex.transaction.ValidationError
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait BurnTransaction extends SignedTransaction {
  def assetId: Array[Byte]
  def amount: Long
  def fee: Long
}

object BurnTransaction extends Deser[BurnTransaction] {

  private case class BurnTransactionImpl(sender: PublicKeyAccount,
                                         assetId: Array[Byte],
                                         amount: Long,
                                         fee: Long,
                                         timestamp: Long,
                                         signature: Array[Byte])
      extends BurnTransaction {

    override val transactionType: TransactionType.Value = TransactionType.BurnTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
                                                sender.publicKey,
                                                assetId,
                                                Longs.toByteArray(amount),
                                                Longs.toByteArray(fee),
                                                Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
        "assetId" -> Base58.encode(assetId),
        "amount"  -> amount,
        "fee"     -> fee
      )

    override val assetFee: (Option[AssetId], Long) = (None, fee)
    override lazy val balanceChanges: Seq[BalanceChange] =
      Seq(BalanceChange(AssetAcc(sender, Some(assetId)), -amount), BalanceChange(AssetAcc(sender, assetFee._1), -assetFee._2))

    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  }
  override def parseBytes(bytes: Array[Byte]): Try[BurnTransaction] = Try {
    require(bytes.head == TransactionType.BurnTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[BurnTransaction] = Try {
    import EllipticCurveImpl._
    val sender        = new PublicKeyAccount(bytes.slice(0, KeyLength))
    val assetId       = bytes.slice(KeyLength, KeyLength + AssetIdLength)
    val quantityStart = KeyLength + AssetIdLength

    val quantity  = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val fee       = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
    val signature = bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength)
    BurnTransaction
      .create(sender, assetId, quantity, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               assetId: Array[Byte],
                               quantity: Long,
                               fee: Long,
                               timestamp: Long,
                               signature: Option[Array[Byte]] = None): Either[ValidationError, BurnTransactionImpl] =
    if (quantity < 0) {
      Left(ValidationError.NegativeAmount)
    } else if (!Account.isValid(sender)) {
      Left(ValidationError.InvalidAddress)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(BurnTransactionImpl(sender, assetId, quantity, fee, timestamp, signature.orNull))
    }

  def create(sender: PublicKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signature: Array[Byte]): Either[ValidationError, BurnTransaction] =
    createUnverified(sender, assetId, quantity, fee, timestamp, Some(signature)).right.flatMap(SignedTransaction.verify)

  def create(sender: PrivateKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             fee: Long,
             timestamp: Long): Either[ValidationError, BurnTransaction] =
    createUnverified(sender, assetId, quantity, fee, timestamp).right.map { unverified =>
      unverified.copy(signature = EllipticCurveImpl.sign(sender, unverified.toSign))
    }
  }
