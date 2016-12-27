package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.TypedTransaction.TransactionType
import scorex.transaction.ValidationResult.ValidationResult
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait TransferTransaction extends SignedTransaction {
  def assetId: Option[AssetId]
  def recipient: Account
  def amount: Long
  def feeAssetId: Option[AssetId]
  def fee: Long
  def attachment: Array[Byte]
}

object TransferTransaction extends Deser[TransferTransaction] {

  private case class TransferTransactionImpl(assetId: Option[AssetId],
                                     sender: PublicKeyAccount,
                                     recipient: Account,
                                     amount: Long,
                                     timestamp: Long,
                                     feeAssetId: Option[AssetId],
                                     fee: Long,
                                     attachment: Array[Byte],
                                     signature: Array[Byte])
      extends TransferTransaction {
    override val transactionType: TransactionType.Value = TransactionType.TransferTransaction

    override def balanceChanges(): Seq[BalanceChange] = {
      val recipientCh = BalanceChange(AssetAcc(recipient, assetId), amount)
      val senderCh =
        if (sameAssetForFee) Seq(BalanceChange(AssetAcc(sender, assetId), -amount - fee))
        else Seq(BalanceChange(AssetAcc(sender, assetId), -amount), BalanceChange(AssetAcc(sender, feeAssetId), -fee))

      recipientCh +: senderCh
    }

    lazy val sameAssetForFee: Boolean = feeAssetId.map(fa => assetId.exists(_ sameElements fa)).getOrElse(assetId.isEmpty)

    override val assetFee: (Option[AssetId], Long) = (feeAssetId, fee)

    lazy val toSign: Array[Byte] = {
      val timestampBytes  = Longs.toByteArray(timestamp)
      val assetIdBytes    = assetId.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
      val amountBytes     = Longs.toByteArray(amount)
      val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
      val feeBytes        = Longs.toByteArray(fee)

      Bytes.concat(Array(transactionType.id.toByte),
                   sender.publicKey,
                   assetIdBytes,
                   feeAssetIdBytes,
                   timestampBytes,
                   amountBytes,
                   feeBytes,
                   recipient.bytes,
                   arrayWithSize(attachment))
    }


    override lazy val json: JsObject = jsonBase() ++ Json.obj(
        "recipient"  -> recipient.address,
        "assetId"    -> assetId.map(Base58.encode),
        "amount"     -> amount,
        "feeAsset"   -> feeAssetId.map(Base58.encode),
        "attachment" -> Base58.encode(attachment)
      )

    override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)

  }

  val MaxAttachmentSize = 140

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    require(bytes.head == TransactionType.TransferTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[TransferTransaction] = Try {
    import EllipticCurveImpl._

    val signature = bytes.slice(0, SignatureLength)
    val txId      = bytes(SignatureLength)
    require(txId == TransactionType.TransferTransaction.id.toByte, s"Signed tx id is not match")
    val sender              = new PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val (assetIdOpt, s0)    = parseOption(bytes, SignatureLength + KeyLength + 1, AssetIdLength)
    val (feeAssetIdOpt, s1) = parseOption(bytes, s0, AssetIdLength)
    val timestamp           = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
    val amount              = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
    val feeAmount           = Longs.fromByteArray(bytes.slice(s1 + 16, s1 + 24))
    val recipient           = new Account(Base58.encode(bytes.slice(s1 + 24, s1 + 24 + Account.AddressLength)))
    val (attachment, _)     = parseArraySize(bytes, s1 + 24 + Account.AddressLength)

    TransferTransaction
      .create(assetIdOpt, sender, recipient, amount, timestamp, feeAssetIdOpt, feeAmount, attachment, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(assetId: Option[AssetId],
             sender: PublicKeyAccount,
             recipient: Account,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte],
             signature: Array[Byte]): Either[ValidationResult, TransferTransaction] = {
    if (!Account.isValid(recipient)) {
      Left(ValidationResult.InvalidAddress) //CHECK IF RECIPIENT IS VALID ADDRESS
    } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
      Left(ValidationResult.TooBigArray)
    } else if (amount <= 0) {
      Left(ValidationResult.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (Try(Math.addExact(amount, feeAmount)).isFailure) {
      Left(ValidationResult.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else if (!Account.isValid(sender)) {
      Left(ValidationResult.InvalidAddress)
    } else if (feeAmount <= 0) {
      Left(ValidationResult.InsufficientFee)
    } else {
      val unsigned = TransferTransactionImpl(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, null)
      if (EllipticCurveImpl.verify(signature, unsigned.toSign, sender.publicKey)) {
        Right(unsigned.copy(signature = signature))
      } else {
        Left(ValidationResult.InvalidSignature)
      }
    }
  }

  def create(assetId: Option[AssetId],
             sender: PrivateKeyAccount,
             recipient: Account,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte]): Either[ValidationResult, TransferTransaction] = {
    if (!Account.isValid(recipient)) {
      Left(ValidationResult.InvalidAddress) //CHECK IF RECIPIENT IS VALID ADDRESS
    } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
      Left(ValidationResult.TooBigArray)
    } else if (amount <= 0) {
      Left(ValidationResult.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (Try(Math.addExact(amount, feeAmount)).isFailure) {
      Left(ValidationResult.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else if (!Account.isValid(sender)) {
      Left(ValidationResult.InvalidAddress)
    } else if (feeAmount <= 0) {
      Left(ValidationResult.InsufficientFee)
    } else {
      val unsigned = TransferTransactionImpl(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, null)
      val sig      = EllipticCurveImpl.sign(sender, unsigned.toSign)
      Right(unsigned.copy(signature = sig))
    }
  }
}
