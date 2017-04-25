package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait MakeUniqueAssetTransaction extends SignedTransaction {
  def assetId: Array[Byte]
  def fee: Long
}

object MakeUniqueAssetTransaction {

  private case class MakeUniqueAssetTransactionImpl(sender: PublicKeyAccount,
                                         assetId: Array[Byte],
                                         fee: Long,
                                         timestamp: Long,
                                         signature: Array[Byte])
    extends MakeUniqueAssetTransaction {

    override val transactionType: TransactionType.Value = TransactionType.MakeUniqueAssetTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
      sender.publicKey,
      assetId,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "assetId" -> Base58.encode(assetId),
      "fee"     -> fee
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)

    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  }
  
  def parseBytes(bytes: Array[Byte]): Try[MakeUniqueAssetTransaction] = Try {
    require(bytes.head == TransactionType.MakeUniqueAssetTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[MakeUniqueAssetTransaction] = Try {
    import EllipticCurveImpl._
    val sender        = PublicKeyAccount(bytes.slice(0, KeyLength))
    val assetId       = bytes.slice(KeyLength, KeyLength + AssetIdLength)
    val quantityStart = KeyLength + AssetIdLength

    val fee       = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val signature = bytes.slice(quantityStart + 16, quantityStart + 16 + SignatureLength)
    MakeUniqueAssetTransaction
      .create(sender, assetId, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               assetId: Array[Byte],
                               fee: Long,
                               timestamp: Long,
                               signature: Option[Array[Byte]] = None): Either[ValidationError, MakeUniqueAssetTransactionImpl] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(MakeUniqueAssetTransactionImpl(sender, assetId, fee, timestamp, signature.orNull))
    }

  def create(sender: PublicKeyAccount,
             assetId: Array[Byte],
             fee: Long,
             timestamp: Long,
             signature: Array[Byte]): Either[ValidationError, MakeUniqueAssetTransaction] =
    createUnverified(sender, assetId, fee, timestamp, Some(signature)).right.flatMap(SignedTransaction.verify)

  def create(sender: PrivateKeyAccount,
             assetId: Array[Byte],
             fee: Long,
             timestamp: Long): Either[ValidationError, MakeUniqueAssetTransaction] =
    createUnverified(sender, assetId, fee, timestamp).right.map { unverified =>
      unverified.copy(signature = EllipticCurveImpl.sign(sender, unverified.toSign))
    }
}
