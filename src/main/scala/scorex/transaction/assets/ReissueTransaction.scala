package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.TransactionParsers._
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class ReissueTransaction private (sender: PublicKeyAccount,
                                       assetId: ByteStr,
                                       quantity: Long,
                                       reissuable: Boolean,
                                       fee: Long,
                                       timestamp: Long,
                                       signature: ByteStr)
    extends SignedTransaction
    with FastHashId {

  override val builder: ReissueTransaction.type = ReissueTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId),
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override def json(implicit addressScheme: AddressScheme): JsObject =
    jsonBase ++ Json.obj(
      "assetId"    -> assetId.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    )

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))
}

object ReissueTransaction extends TransactionParserFor[ReissueTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 5

  override protected def parseTail(version: Byte, bytes: Array[Byte])(implicit addressScheme: AddressScheme): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val sender        = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
      val assetId       = ByteStr(bytes.slice(SignatureLength + KeyLength + 1, SignatureLength + KeyLength + AssetIdLength + 1))
      val quantityStart = SignatureLength + KeyLength + AssetIdLength + 1

      val quantity   = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
      val reissuable = bytes.slice(quantityStart + 8, quantityStart + 9).head == (1: Byte)
      val fee        = Longs.fromByteArray(bytes.slice(quantityStart + 9, quantityStart + 17))
      val timestamp  = Longs.fromByteArray(bytes.slice(quantityStart + 17, quantityStart + 25))
      ReissueTransaction
        .create(sender, assetId, quantity, reissuable, fee, timestamp, signature)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] =
    if (quantity <= 0) {
      Left(ValidationError.NegativeAmount(quantity, "assets"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(ReissueTransaction(sender, assetId, quantity, reissuable, fee, timestamp, signature))
    }

  def create(sender: PrivateKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long): Either[ValidationError, TransactionT] =
    create(sender, assetId, quantity, reissuable, fee, timestamp, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
}
