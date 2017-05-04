package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

sealed trait MakeAssetNameUniqueTransaction extends SignedTransaction {
  def assetId: Array[Byte]
  def fee: Long
}

object MakeAssetNameUniqueTransaction {

  private case class MakeAssetNameUniqueTransactionImpl(sender: PublicKeyAccount,
                                                        assetId: Array[Byte],
                                                        fee: Long,
                                                        timestamp: Long,
                                                        networkByte: Byte,
                                                        signature: Array[Byte])
    extends MakeAssetNameUniqueTransaction {

    override val transactionType: TransactionType.Value = TransactionType.MakeAssetNameUniqueTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte, networkByte),
      sender.publicKey,
      assetId,
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "assetId" -> Base58.encode(assetId),
      "fee"     -> fee
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)

    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  }

  def parseBytes(bytes: Array[Byte]): Try[MakeAssetNameUniqueTransaction] = Try {
    require(bytes.head == TransactionType.MakeAssetNameUniqueTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[MakeAssetNameUniqueTransaction] = Try {
    import EllipticCurveImpl._
    val networkByte   = bytes.head
    val sender        = PublicKeyAccount(bytes.slice(1, 1 + KeyLength))
    val assetId       = bytes.slice(1 + KeyLength, 1 + KeyLength + AssetIdLength)
    val quantityStart = 1 + KeyLength + AssetIdLength

    val fee       = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val signature = bytes.slice(quantityStart + 16, quantityStart + 16 + SignatureLength)
    MakeAssetNameUniqueTransaction
      .create(sender, assetId, fee, timestamp, networkByte, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               assetId: Array[Byte],
                               fee: Long,
                               timestamp: Long,
                               networkByte: Byte,
                               signature: Option[Array[Byte]] = None): Either[ValidationError, MakeAssetNameUniqueTransactionImpl] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(MakeAssetNameUniqueTransactionImpl(sender, assetId, fee, timestamp, networkByte, signature.orNull))
    }

  def create(sender: PublicKeyAccount,
             assetId: Array[Byte],
             fee: Long,
             timestamp: Long,
             networkByte: Byte,
             signature: Array[Byte]): Either[ValidationError, MakeAssetNameUniqueTransaction] =
    createUnverified(sender, assetId, fee, timestamp, networkByte, Some(signature)).right.flatMap(SignedTransaction.verify)

  def create(sender: PrivateKeyAccount,
             assetId: Array[Byte],
             fee: Long,
             networkByte: Byte,
             timestamp: Long): Either[ValidationError, MakeAssetNameUniqueTransaction] =
    createUnverified(sender, assetId, fee, timestamp, networkByte).right.map { unverified =>
      unverified.copy(signature = EllipticCurveImpl.sign(sender, unverified.toSign))
    }
}
