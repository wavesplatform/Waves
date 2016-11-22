package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.TypedTransaction.TransactionType
import scorex.transaction._

import scala.util.Try

case class DeleteTransaction(sender: PublicKeyAccount,
                             assetId: Array[Byte],
                             amount: Long,
                             fee: Long,
                             timestamp: Long,
                             signature: Array[Byte]) extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.DeleteTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), sender.publicKey, assetId,
    Longs.toByteArray(amount), Longs.toByteArray(fee), Longs.toByteArray(timestamp))

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "assetId" -> Base58.encode(assetId),
    "amount" -> amount,
    "fee" -> fee
  )

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override lazy val balanceChanges: Seq[BalanceChange] =
    Seq(BalanceChange(AssetAcc(sender, Some(assetId)), -amount),
      BalanceChange(AssetAcc(sender, assetFee._1), -assetFee._2))

  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  //TODO move to parent class
  def validate: ValidationResult.Value =
  if (!Account.isValid(sender)) {
    ValidationResult.InvalidAddress
  } else if (amount <= 0) {
    ValidationResult.NegativeAmount
  } else if (fee <= 0) {
    ValidationResult.InsufficientFee
  } else if (!signatureValid) {
    ValidationResult.InvalidSignature
  } else ValidationResult.ValidateOke

}


object DeleteTransaction extends Deser[DeleteTransaction] {

  override def parseBytes(bytes: Array[Byte]): Try[DeleteTransaction] = Try {
    require(bytes.head == TransactionType.DeleteTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[DeleteTransaction] = Try {
    import EllipticCurveImpl._
    val sender = new PublicKeyAccount(bytes.slice(0, KeyLength))
    val assetId = bytes.slice(KeyLength, KeyLength + AssetIdLength)
    val quantityStart = KeyLength + AssetIdLength

    val quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val fee = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
    val signature = bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength)
    DeleteTransaction(sender, assetId, quantity, fee, timestamp, signature)
  }

  def create(sender: PrivateKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             fee: Long,
             timestamp: Long): DeleteTransaction = {
    val unsigned = DeleteTransaction(sender, assetId, quantity, fee, timestamp, null)
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }
}
