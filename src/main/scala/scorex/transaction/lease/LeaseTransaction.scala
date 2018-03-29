package scorex.transaction.lease

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account._
import scorex.transaction.TransactionParsers._
import scorex.transaction._

import scala.util.{Failure, Success, Try}

case class LeaseTransaction private (sender: PublicKeyAccount,
                                     amount: Long,
                                     fee: Long,
                                     timestamp: Long,
                                     recipient: AddressOrAlias,
                                     signature: ByteStr)
    extends SignedTransaction
    with FastHashId {

  override val builder: TransactionParser = LeaseTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(Array(builder.typeId),
                 sender.publicKey,
                 recipient.bytes.arr,
                 Longs.toByteArray(amount),
                 Longs.toByteArray(fee),
                 Longs.toByteArray(timestamp)))

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "amount"    -> amount,
      "recipient" -> recipient.stringRepr,
      "fee"       -> fee,
      "timestamp" -> timestamp
    ))

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val bytes: Coeval[Array[Byte]]        = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))

}

object LeaseTransaction extends TransactionParserFor[LeaseTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 8

  object Status {
    val Active   = "active"
    val Canceled = "canceled"
  }

  override protected def parseTail(version: Byte, bytes: Array[Byte])(implicit addressScheme: AddressScheme): Try[TransactionT] =
    Try {
      val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
      (for {
        recRes <- AddressOrAlias.fromBytes(bytes, KeyLength)
        (recipient, recipientEnd) = recRes
        quantityStart             = recipientEnd
        quantity                  = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
        fee                       = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
        timestamp                 = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))
        signature                 = ByteStr(bytes.slice(quantityStart + 24, quantityStart + 24 + SignatureLength))
        lt <- LeaseTransaction.create(sender, quantity, fee, timestamp, recipient, signature)
      } yield lt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(sender: PublicKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias, signature: ByteStr)(
      implicit addressScheme: AddressScheme): Either[ValidationError, TransactionT] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount(amount, "waves"))
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else if (recipient.isInstanceOf[Address] && sender.toAddress.stringRepr == recipient.stringRepr) {
      Left(ValidationError.ToSelf)
    } else {
      Right(LeaseTransaction(sender, amount, fee, timestamp, recipient, signature))
    }
  }

  def create(sender: PrivateKeyAccount, amount: Long, fee: Long, timestamp: Long, recipient: AddressOrAlias)(
      implicit addressScheme: AddressScheme): Either[ValidationError, TransactionT] = {
    create(sender, amount, fee, timestamp, recipient, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
  }
}
