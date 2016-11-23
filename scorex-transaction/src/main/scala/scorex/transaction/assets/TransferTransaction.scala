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

case class TransferTransaction(assetId: Option[AssetId],
                               sender: PublicKeyAccount,
                               recipient: Account,
                               amount: Long,
                               timestamp: Long,
                               feeAssetId: Option[AssetId],
                               fee: Long,
                               attachment: Array[Byte],
                               signature: Array[Byte]) extends SignedTransaction {
  override val transactionType: TransactionType.Value = TransactionType.TransferTransaction

  override def balanceChanges(): Seq[BalanceChange] = {
    val recipientCh = BalanceChange(AssetAcc(recipient, assetId), amount)
    val senderCh = if (sameAssetForFee) Seq(BalanceChange(AssetAcc(sender, assetId), -amount - fee))
    else Seq(BalanceChange(AssetAcc(sender, assetId), -amount), BalanceChange(AssetAcc(sender, feeAssetId), -fee))

    recipientCh +: senderCh
  }

  lazy val sameAssetForFee: Boolean = feeAssetId.map(fa => assetId.exists(_ sameElements fa)).getOrElse(assetId.isEmpty)

  override val assetFee: (Option[AssetId], Long) = (feeAssetId, fee)

  lazy val toSign: Array[Byte] = {
    val timestampBytes = Longs.toByteArray(timestamp)
    val assetIdBytes = assetId.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
    val amountBytes = Longs.toByteArray(amount)
    val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
    val feeBytes = Longs.toByteArray(fee)

    Bytes.concat(Array(transactionType.id.toByte), sender.publicKey, assetIdBytes, feeAssetIdBytes, timestampBytes, amountBytes, feeBytes,
      recipient.bytes, arrayWithSize(attachment))
  }

  def validate: ValidationResult.Value = if (!Account.isValid(recipient)) {
    ValidationResult.InvalidAddress //CHECK IF RECIPIENT IS VALID ADDRESS
  } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
    ValidationResult.TooBigArray
  } else if (amount <= 0) {
    ValidationResult.NegativeAmount //CHECK IF AMOUNT IS POSITIVE
  } else if (fee <= 0) {
    ValidationResult.InsufficientFee //CHECK IF FEE IS POSITIVE
  } else if (Try(Math.addExact(amount, fee)).isFailure) {
    ValidationResult.OverflowError // CHECK THAT fee+amount won't overflow Long
  } else if (!signatureValid) {
    ValidationResult.InvalidSignature
  } else ValidationResult.ValidateOke


  override lazy val json: JsObject = Json.obj(
    "type" -> transactionType.id,
    "id" -> Base58.encode(id),
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "recipient" -> recipient.address,
    "assetId" -> assetId.map(Base58.encode),
    "amount" -> amount,
    "feeAssetId" -> feeAssetId.map(Base58.encode),
    "fee" -> fee,
    "timestamp" -> timestamp,
    "attachment" -> Base58.encode(attachment),
    "signature" -> Base58.encode(signature)
  )

  override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)

}

object TransferTransaction extends Deser[TransferTransaction] {

  val MaxAttachmentSize = 140

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    require(bytes.head == TransactionType.TransferTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    import EllipticCurveImpl._

    val signature = bytes.slice(0, SignatureLength)
    val txId = bytes(SignatureLength)
    require(txId == TransactionType.TransferTransaction.id.toByte, s"Signed tx id is not match")
    val sender = new PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val (assetIdOpt, s0) = parseOption(bytes, SignatureLength + KeyLength + 1, AssetIdLength)
    val (feeAssetIdOpt, s1) = parseOption(bytes, s0, AssetIdLength)
    val timestamp = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
    val amount = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
    val feeAmount = Longs.fromByteArray(bytes.slice(s1 + 16, s1 + 24))
    val recipient = new Account(Base58.encode(bytes.slice(s1 + 24, s1 + 24 + Account.AddressLength)))
    val (attachment, _) = parseArraySize(bytes, s1 + 24 + Account.AddressLength)

    TransferTransaction(assetIdOpt, sender, recipient, amount, timestamp, feeAssetIdOpt, feeAmount, attachment, signature)
  }

  def create(assetId: Option[AssetId],
             sender: PrivateKeyAccount,
             recipient: Account,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte]): TransferTransaction = {
    val unsigned = TransferTransaction(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, null)
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }
}
