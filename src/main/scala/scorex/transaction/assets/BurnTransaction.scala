package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class BurnTransaction private (sender: PublicKeyAccount, assetId: ByteStr, amount: Long, fee: Long, timestamp: Long, signature: ByteStr)
    extends SignedTransaction
    with FastHashId {

  override val builder: BurnTransaction.type = BurnTransaction
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes
      .concat(Array(builder.typeId), sender.publicKey, assetId.arr, Longs.toByteArray(amount), Longs.toByteArray(fee), Longs.toByteArray(timestamp)))

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "assetId" -> assetId.base58,
      "amount"  -> amount,
      "fee"     -> fee
    )
  }

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))

}

object BurnTransaction extends TransactionParserFor[BurnTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 6

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val sender        = PublicKeyAccount(bytes.slice(0, KeyLength))
      val assetId       = ByteStr(bytes.slice(KeyLength, KeyLength + AssetIdLength))
      val quantityStart = KeyLength + AssetIdLength

      val quantity  = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
      val fee       = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
      val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
      val signature = ByteStr(bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength))
      BurnTransaction
        .create(sender, assetId, quantity, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] =
    if (quantity < 0) {
      Left(ValidationError.NegativeAmount(quantity, "assets"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else {
      Right(BurnTransaction(sender, assetId, quantity, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount, assetId: ByteStr, quantity: Long, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] =
    create(sender, assetId, quantity, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(sender, unverified.bodyBytes())))
    }
}
