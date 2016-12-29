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

sealed trait ReissueTransaction extends AssetIssuance {
  def fee: Long
}

object ReissueTransaction extends Deser[ReissueTransaction] {

  private case class ReissueTransactionImpl(sender: PublicKeyAccount,
                                    assetId: Array[Byte],
                                    quantity: Long,
                                    reissuable: Boolean,
                                    fee: Long,
                                    timestamp: Long,
                                    signature: Array[Byte])
      extends ReissueTransaction {

    override val transactionType: TransactionType.Value = TransactionType.ReissueTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
                                                sender.publicKey,
                                                assetId,
                                                Longs.toByteArray(quantity),
                                                if (reissuable) Array(1: Byte) else Array(0: Byte),
                                                Longs.toByteArray(fee),
                                                Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
        "assetId"    -> Base58.encode(assetId),
        "quantity"   -> quantity,
        "reissuable" -> reissuable
      )

    override val assetFee: (Option[AssetId], Long) = (None, fee)
    override lazy val balanceChanges: Seq[BalanceChange] =
      Seq(BalanceChange(AssetAcc(sender, Some(assetId)), quantity), BalanceChange(AssetAcc(sender, assetFee._1), -assetFee._2))

    override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)

  }

  override def parseBytes(bytes: Array[Byte]): Try[ReissueTransaction] = Try {
    require(bytes.head == TransactionType.ReissueTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[ReissueTransaction] = Try {
    import EllipticCurveImpl._
    val signature = bytes.slice(0, SignatureLength)
    val txId      = bytes(SignatureLength)
    require(txId == TransactionType.ReissueTransaction.id.toByte, s"Signed tx id is not match")
    val sender        = new PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val assetId       = bytes.slice(SignatureLength + KeyLength + 1, SignatureLength + KeyLength + AssetIdLength + 1)
    val quantityStart = SignatureLength + KeyLength + AssetIdLength + 1

    val quantity   = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val reissuable = bytes.slice(quantityStart + 8, quantityStart + 9).head == (1: Byte)
    val fee        = Longs.fromByteArray(bytes.slice(quantityStart + 9, quantityStart + 17))
    val timestamp  = Longs.fromByteArray(bytes.slice(quantityStart + 17, quantityStart + 25))
    ReissueTransaction.create(sender, assetId, quantity, reissuable, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: Array[Byte]): Either[ValidationResult, ReissueTransaction] = {
    if (quantity <= 0) {
      Left(ValidationResult.NegativeAmount)
    } else if (!Account.isValid(sender)) {
      Left(ValidationResult.InvalidAddress)
    } else if (fee <= 0) {
      Left(ValidationResult.InsufficientFee)
    } else {
      val unsigned = ReissueTransactionImpl(sender, assetId, quantity, reissuable, fee, timestamp, null)
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
             reissuable: Boolean,
             fee: Long,
             timestamp: Long): Either[ValidationResult, ReissueTransaction] = {
    if (quantity <= 0) {
      Left(ValidationResult.NegativeAmount)
    } else if (!Account.isValid(sender)) {
      Left(ValidationResult.InvalidAddress)
    } else if (fee <= 0) {
      Left(ValidationResult.InsufficientFee)
    } else {
      val unsigned = ReissueTransactionImpl(sender, assetId, quantity, reissuable, fee, timestamp, null)
      val sig      = EllipticCurveImpl.sign(sender, unsigned.toSign)
      Right(unsigned.copy(signature = sig))
    }
  }
}
