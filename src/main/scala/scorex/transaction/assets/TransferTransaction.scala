package scorex.transaction.assets

import scala.util.{Failure, Success, Try}
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TypedTransaction._
import scorex.transaction.{ValidationError, _}

sealed trait TransferTransaction extends SignedTransaction {
  def assetId: Option[AssetId]

  def recipient: Account

  def amount: Long

  def feeAssetId: Option[AssetId]

  def fee: Long

  def attachment: Array[Byte]
}

object TransferTransaction extends Deser[TransferTransaction] {

  val MaxAttachmentSize = 140
  val MaxAttachmentStringSize = base58Length(MaxAttachmentSize)

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
      val timestampBytes = Longs.toByteArray(timestamp)
      val assetIdBytes = assetId.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
      val amountBytes = Longs.toByteArray(amount)
      val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
      val feeBytes = Longs.toByteArray(fee)

      Bytes.concat(Array(transactionType.id.toByte),
        sender.publicKey,
        assetIdBytes,
        feeAssetIdBytes,
        timestampBytes,
        amountBytes,
        feeBytes,
        recipient.bytes,
        BytesSerializable.arrayWithSize(attachment))
    }


    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "recipient" -> recipient.address,
      "assetId" -> assetId.map(Base58.encode),
      "amount" -> amount,
      "feeAsset" -> feeAssetId.map(Base58.encode),
      "attachment" -> Base58.encode(attachment)
    )

    override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)

  }

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

    TransferTransaction
      .create(assetIdOpt, sender, recipient, amount, timestamp, feeAssetIdOpt, feeAmount, attachment, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(assetId: Option[AssetId],
                               sender: PublicKeyAccount,
                               recipient: Account,
                               amount: Long,
                               timestamp: Long,
                               feeAssetId: Option[AssetId],
                               feeAmount: Long,
                               attachment: Array[Byte],
                               signature: Option[Array[Byte]] = None) = {
    if (!Account.isValid(recipient)) {
      Left(ValidationError.InvalidAddress) //CHECK IF RECIPIENT IS VALID ADDRESS
    } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
      Left(ValidationError.TooBigArray)
    } else if (amount <= 0) {
      Left(ValidationError.NegativeAmount) //CHECK IF AMOUNT IS POSITIVE
    } else if (Try(Math.addExact(amount, feeAmount)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else if (!Account.isValid(sender)) {
      Left(ValidationError.InvalidAddress)
    } else if (feeAmount <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(TransferTransactionImpl(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, signature.orNull))
    }
  }

  def create(assetId: Option[AssetId],
             sender: PublicKeyAccount,
             recipient: Account,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte],
             signature: Array[Byte]): Either[ValidationError, TransferTransaction] = {
    createUnverified(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, Some(signature))
      .right.flatMap(SignedTransaction.verify)
  }

  def create(assetId: Option[AssetId],
             sender: PrivateKeyAccount,
             recipient: Account,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte]): Either[ValidationError, TransferTransaction] = {
    createUnverified(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment).right.map { unsigned =>
      unsigned.copy(signature = EllipticCurveImpl.sign(sender, unsigned.toSign))
    }
  }
}
