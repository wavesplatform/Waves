package scorex.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}


case class CreateAliasTransaction private(sender: PublicKeyAccount,
                                          alias: Alias,
                                          fee: Long,
                                          timestamp: Long,
                                          signature: ByteStr)
  extends SignedTransaction {

  override val transactionType: TransactionType.Value = TransactionType.CreateAliasTransaction

  override val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(FastCryptographicHash(transactionType.id.toByte +: alias.bytes.arr)))

  override val toSign: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(
    Array(transactionType.id.toByte),
    sender.publicKey,
    BytesSerializable.arrayWithSize(alias.bytes.arr),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp)))

  override val json: Coeval[JsObject] = Coeval.evalOnce(jsonBase() ++ Json.obj(
    "alias" -> alias.name,
    "fee" -> fee,
    "timestamp" -> timestamp
  ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(toSign(), signature.arr))

}

object CreateAliasTransaction {

  def parseTail(bytes: Array[Byte]): Try[CreateAliasTransaction] = Try {
    import EllipticCurveImpl._
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val (aliasBytes, aliasEnd) = Deser.parseArraySize(bytes, KeyLength)
    (for {
      alias <- Alias.fromBytes(aliasBytes)
      fee = Longs.fromByteArray(bytes.slice(aliasEnd, aliasEnd + 8))
      timestamp = Longs.fromByteArray(bytes.slice(aliasEnd + 8, aliasEnd + 16))
      signature = ByteStr(bytes.slice(aliasEnd + 16, aliasEnd + 16 + SignatureLength))
      tx <- CreateAliasTransaction.create(sender, alias, fee, timestamp, signature)
    } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(sender: PublicKeyAccount,
             alias: Alias,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, CreateAliasTransaction] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(CreateAliasTransaction(sender, alias, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             alias: Alias,
             fee: Long,
             timestamp: Long): Either[ValidationError, CreateAliasTransaction] = {
    create(sender, alias, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign())))
    }
  }
}
