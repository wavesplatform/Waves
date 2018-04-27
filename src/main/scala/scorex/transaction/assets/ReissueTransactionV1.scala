package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}
import scorex.transaction._

import scala.util.{Failure, Success, Try}

trait ReissueTransaction extends ProvenTransaction {
  def assetId: ByteStr
  def quantity: Long
  def reissuable: Boolean
  def fee: Long

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "assetId"    -> assetId.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    ))

  protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
}

object ReissueTransaction {
  def validateReissueParams(quantity: Long, fee: Long): Either[ValidationError, Unit] =
    if (quantity <= 0) {
      Left(ValidationError.NegativeAmount(quantity, "assets"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else Right(())

  def parseBase(bytes: Array[Byte], start: Int): (PublicKeyAccount, AssetId, Long, Boolean, Long, Long, Int) = {
    val sender        = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val assetId       = ByteStr(bytes.slice(SignatureLength + KeyLength + 1, SignatureLength + KeyLength + AssetIdLength + 1))
    val quantityStart = SignatureLength + KeyLength + AssetIdLength + 1
    val quantity      = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val reissuable    = bytes.slice(quantityStart + 8, quantityStart + 9).head == (1: Byte)
    val fee           = Longs.fromByteArray(bytes.slice(quantityStart + 9, quantityStart + 17))
    val end           = quantityStart + 25
    val timestamp     = Longs.fromByteArray(bytes.slice(quantityStart + 17, end))

    (sender, assetId, quantity, reissuable, fee, timestamp, end)
  }
}

case class ReissueTransactionV1 private (sender: PublicKeyAccount,
                                         assetId: ByteStr,
                                         quantity: Long,
                                         reissuable: Boolean,
                                         fee: Long,
                                         timestamp: Long,
                                         signature: ByteStr)
    extends ReissueTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: ReissueTransactionV1.type = ReissueTransactionV1
  val bodyBytes: Coeval[Array[Byte]]              = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), bytesBase()))
  override val bytes: Coeval[Array[Byte]]         = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))
}

object ReissueTransactionV1 extends TransactionParserFor[ReissueTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 5

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val (sender, assetId, quantity, reissuable, fee, timestamp, _) = ReissueTransaction.parseBase(bytes, SignatureLength + 1)
      ReissueTransactionV1
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
    ReissueTransaction
      .validateReissueParams(quantity, fee)
      .map(_ => ReissueTransactionV1(sender, assetId, quantity, reissuable, fee, timestamp, signature))

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
