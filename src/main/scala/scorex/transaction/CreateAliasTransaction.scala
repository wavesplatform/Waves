package scorex.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}

sealed trait CreateAliasTransaction extends SignedTransaction {
  def alias: Alias

  def fee: Long
}

object CreateAliasTransaction {

  private case class CreateAliasTransactionImpl(sender: PublicKeyAccount,
                                                alias: Alias,
                                                fee: Long,
                                                timestamp: Long,
                                                signature: ByteStr)
    extends CreateAliasTransaction {

    override val transactionType: TransactionType.Value = TransactionType.CreateAliasTransaction

    override lazy val id: ByteStr = ByteStr(FastCryptographicHash(transactionType.id.toByte +: alias.bytes.arr))

    lazy val toSign: Array[Byte] = Bytes.concat(
      Array(transactionType.id.toByte),
      sender.publicKey,
      BytesSerializable.arrayWithSize(alias.bytes.arr),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "alias" -> alias.name,
      "fee" -> fee,
      "timestamp" -> timestamp
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)
    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature.arr)

  }

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

  private def createUnverified(sender: PublicKeyAccount,
                               alias: Alias,
                               fee: Long,
                               timestamp: Long,
                               signature: Option[ByteStr] = None): Either[ValidationError, CreateAliasTransactionImpl] = {
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(CreateAliasTransactionImpl(sender, alias, fee, timestamp, signature.orNull))
    }
  }

  def create(sender: PublicKeyAccount,
             alias: Alias,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, CreateAliasTransaction] = {
    createUnverified(sender, alias, fee, timestamp, Some(signature))
  }

  def create(sender: PrivateKeyAccount,
             alias: Alias,
             fee: Long,
             timestamp: Long): Either[ValidationError, CreateAliasTransaction] = {
    createUnverified(sender, alias, fee, timestamp).right.map { unsigned =>
      unsigned.copy(signature =ByteStr(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
  }
}
