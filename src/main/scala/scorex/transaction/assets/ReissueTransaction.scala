package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.{ByteArray, EqByteArray}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser._
import scorex.transaction.{ValidationError, _}

import scala.util.{Failure, Success, Try}

sealed trait ReissueTransaction extends AssetIssuance {
  def fee: Long
}

object ReissueTransaction {

  private case class ReissueTransactionImpl(sender: PublicKeyAccount,
                                            assetId: Array[Byte],
                                            quantity: Long,
                                            reissuable: Boolean,
                                            fee: Long,
                                            timestamp: Long,
                                            signature: ByteArray)
    extends ReissueTransaction {

    override val transactionType: TransactionType.Value = TransactionType.ReissueTransaction

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
      sender.publicKey,
      assetId,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "assetId" -> Base58.encode(assetId),
      "quantity" -> quantity,
      "reissuable" -> reissuable
    )

    override val assetFee: (Option[AssetId], Long) = (None, fee)

    override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature.arr, toSign)

  }


  def parseTail(bytes: Array[Byte]): Try[ReissueTransaction] = Try {
    import EllipticCurveImpl._
    val signature =EqByteArray(bytes.slice(0, SignatureLength))
    val txId = bytes(SignatureLength)
    require(txId == TransactionType.ReissueTransaction.id.toByte, s"Signed tx id is not match")
    val sender = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val assetId = bytes.slice(SignatureLength + KeyLength + 1, SignatureLength + KeyLength + AssetIdLength + 1)
    val quantityStart = SignatureLength + KeyLength + AssetIdLength + 1

    val quantity = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val reissuable = bytes.slice(quantityStart + 8, quantityStart + 9).head == (1: Byte)
    val fee = Longs.fromByteArray(bytes.slice(quantityStart + 9, quantityStart + 17))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 17, quantityStart + 25))
    ReissueTransaction.create(sender, assetId, quantity, reissuable, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               assetId: Array[Byte],
                               quantity: Long,
                               reissuable: Boolean,
                               fee: Long,
                               timestamp: Long,
                               signature: Option[ByteArray] = None) =
    if (quantity <= 0) {
      Left(ValidationError.NegativeAmount)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(ReissueTransactionImpl(sender, assetId, quantity, reissuable, fee, timestamp, signature.orNull))
    }

  def create(sender: PublicKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signature: ByteArray): Either[ValidationError, ReissueTransaction] =
    createUnverified(sender, assetId, quantity, reissuable, fee, timestamp, Some(signature))
      .right
      .flatMap(SignedTransaction.verify)


  def create(sender: PrivateKeyAccount,
             assetId: Array[Byte],
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long): Either[ValidationError, ReissueTransaction] =
    createUnverified(sender, assetId, quantity, reissuable, fee, timestamp).right.map { unsigned =>
      unsigned.copy(signature = EqByteArray(EllipticCurveImpl.sign(sender, unsigned.toSign)))
    }
}
