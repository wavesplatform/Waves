package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.TypedTransaction.TransactionType
import scorex.transaction.ValidationResult.ValidationResult
import scorex.transaction._

import scala.util.Try

sealed trait DeleteTransaction extends SignedTransaction {
  def assetId: Array[Byte]
  def amount: Long
  def fee: Long
}

object DeleteTransaction extends Deser[DeleteTransaction] {

  private case class DeleteTransactionImpl(sender: PublicKeyAccount,
                                           assetId: Array[Byte],
                                           amount: Long,
                                           fee: Long,
                                           timestamp: Long,
                                           signature: Array[Byte])
      extends DeleteTransaction {

    override val transactionType: TransactionType.Value = TransactionType.DeleteTransaction

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

    override lazy val validate: ValidationResult.Value = if (amount < 0) {
      ValidationResult.NegativeAmount
    } else validationBase

  }
  override def parseBytes(bytes: Array[Byte]): Try[DeleteTransaction] = Try {
    require(bytes.head == TransactionType.DeleteTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[DeleteTransaction] = Try {
    import EllipticCurveImpl._
    val sender        = new PublicKeyAccount(bytes.slice(0, KeyLength))
    val assetId       = bytes.slice(KeyLength, KeyLength + AssetIdLength)
    val quantityStart = KeyLength + AssetIdLength

    val quantity  = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val fee       = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
    val signature = bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength)
    DeleteTransactionImpl(sender, assetId, quantity, fee, timestamp, signature)
  }

  def create(sender: PublicKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signature: Array[Byte]): Either[ValidationResult, DeleteTransaction] = {

    if (quantity < 0) {
      Left(ValidationResult.NegativeAmount)
    } else {
      val unsigned = DeleteTransactionImpl(sender, assetId, quantity, fee, timestamp, null)
      if (EllipticCurveImpl.verify(signature, unsigned.toSign, sender.publicKey)) {
        Right(unsigned.copy(signature = signature))
      } else {
        Left(ValidationResult.InvalidSignature)
      }
    }
  }

  def create(sender: PrivateKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             fee: Long,
             timestamp: Long): Either[ValidationResult, DeleteTransaction] = {
    if (quantity < 0) {
      Left(ValidationResult.NegativeAmount)
    } else {
      val unsigned = DeleteTransactionImpl(sender, assetId, quantity, fee, timestamp, null)
      val sig      = EllipticCurveImpl.sign(sender, unsigned.toSign)
      Right(unsigned.copy(signature = sig))
    }
  }
}
