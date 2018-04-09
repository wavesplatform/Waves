package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class LeaseCancelTransaction private (sender: PublicKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long, signature: ByteStr)
    extends SignedTransaction
    with FastHashId {

  override val builder: TransactionParser = LeaseCancelTransaction

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(Bytes.concat(Array(builder.typeId), sender.publicKey, Longs.toByteArray(fee), Longs.toByteArray(timestamp), leaseId.arr))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "fee"       -> fee,
      "timestamp" -> timestamp,
      "leaseId"   -> leaseId.base58
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val bytes                             = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))

}

object LeaseCancelTransaction extends TransactionParserFor[LeaseCancelTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 9

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val sender    = PublicKeyAccount(bytes.slice(0, KeyLength))
      val fee       = Longs.fromByteArray(bytes.slice(KeyLength, KeyLength + 8))
      val timestamp = Longs.fromByteArray(bytes.slice(KeyLength + 8, KeyLength + 16))
      val leaseId   = ByteStr(bytes.slice(KeyLength + 16, KeyLength + 16 + crypto.DigestSize))
      val signature = ByteStr(bytes.slice(KeyLength + 16 + crypto.DigestSize, KeyLength + 16 + crypto.DigestSize + SignatureLength))
      LeaseCancelTransaction
        .create(sender, leaseId, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long, signature: ByteStr): Either[ValidationError, TransactionT] =
    if (leaseId.arr.length != crypto.DigestSize) {
      Left(ValidationError.GenericError("Lease transaction id is invalid"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(LeaseCancelTransaction(sender, leaseId, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    create(sender, leaseId, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
  }
}
