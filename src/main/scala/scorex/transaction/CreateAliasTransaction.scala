package scorex.transaction

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.serialization.Deser
import scorex.transaction.TransactionParsers._

import scala.util.{Failure, Success, Try}

case class CreateAliasTransaction private (sender: PublicKeyAccount, alias: Alias, fee: Long, timestamp: Long, signature: ByteStr)
    extends SignedTransaction {

  override val builder: TransactionParser = CreateAliasTransaction
  override val id: Coeval[AssetId]        = Coeval.evalOnce(ByteStr(crypto.fastHash(builder.typeId +: alias.bytes.arr)))

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes
      .concat(Array(builder.typeId), sender.publicKey, Deser.serializeArray(alias.bytes.arr), Longs.toByteArray(fee), Longs.toByteArray(timestamp)))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "alias"     -> alias.name,
      "fee"       -> fee,
      "timestamp" -> timestamp
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val bytes: Coeval[Array[Byte]]        = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))

}

object CreateAliasTransaction extends TransactionParserFor[CreateAliasTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 10

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val sender                 = PublicKeyAccount(bytes.slice(0, KeyLength))
      val (aliasBytes, aliasEnd) = Deser.parseArraySize(bytes, KeyLength)
      (for {
        alias <- Alias.fromBytes(aliasBytes)
        fee       = Longs.fromByteArray(bytes.slice(aliasEnd, aliasEnd + 8))
        timestamp = Longs.fromByteArray(bytes.slice(aliasEnd + 8, aliasEnd + 16))
        signature = ByteStr(bytes.slice(aliasEnd + 16, aliasEnd + 16 + SignatureLength))
        tx <- CreateAliasTransaction.create(sender, alias, fee, timestamp, signature)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount, alias: Alias, fee: Long, timestamp: Long, signature: ByteStr): Either[ValidationError, TransactionT] =
    if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(CreateAliasTransaction(sender, alias, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount, alias: Alias, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    create(sender, alias, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
  }
}
