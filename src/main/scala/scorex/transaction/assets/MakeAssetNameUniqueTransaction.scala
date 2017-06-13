package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.TransactionParser.{KeyLength, TransactionType}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

private case class MakeAssetNameUniqueTransaction(sender: PublicKeyAccount,
                                                  assetId: ByteStr,
                                                  fee: Long,
                                                  timestamp: Long,
                                                  networkByte: Byte,
                                                  signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.MakeAssetNameUniqueTransaction

  lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte, networkByte),
    sender.publicKey,
    assetId.arr,
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp)
  )

  override lazy val json: JsObject = jsonBase() ++ Json.obj(
    "assetId" -> assetId.base58,
    "fee" -> fee,
    "networkByte" -> networkByte
  )

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)
}

object MakeAssetNameUniqueTransaction {

  def parseBytes(bytes: Array[Byte]): Try[MakeAssetNameUniqueTransaction] = Try {
    require(bytes.head == TransactionType.MakeAssetNameUniqueTransaction.id)
    parseTail(bytes.tail).get
  }

  def parseTail(bytes: Array[Byte]): Try[MakeAssetNameUniqueTransaction] = Try {
    import EllipticCurveImpl._
    val networkByte = bytes.head
    val sender = PublicKeyAccount(bytes.slice(1, 1 + KeyLength))
    val assetId = ByteStr(bytes.slice(1 + KeyLength, 1 + KeyLength + AssetIdLength))
    val quantityStart = 1 + KeyLength + AssetIdLength

    val fee = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val signature = ByteStr(bytes.slice(quantityStart + 16, quantityStart + 16 + SignatureLength))
    MakeAssetNameUniqueTransaction
      .create(sender, assetId, fee, timestamp, networkByte, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             assetId: ByteStr,
             fee: Long,
             timestamp: Long,
             networkByte: Byte,
             signature: ByteStr): Either[ValidationError, MakeAssetNameUniqueTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(MakeAssetNameUniqueTransaction(sender, assetId, fee, timestamp, networkByte, signature))
    }

  def create(sender: PrivateKeyAccount,
             assetId: ByteStr,
             fee: Long,
             networkByte: Byte,
             timestamp: Long): Either[ValidationError, MakeAssetNameUniqueTransaction] =
    create(sender, assetId, fee, timestamp, networkByte, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unverified.toSign)))
    }
}
