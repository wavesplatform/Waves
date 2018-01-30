package scorex.transaction.Data

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.EllipticCurveImpl.SignatureLength
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction.TransactionParser._
import scorex.transaction._

import scala.util.{Failure, Success, Try}

/**
  * Created by DN on 30/05/2017.
  */
case class DataTransaction(sender: PublicKeyAccount,
                           data: Array[Byte],
                           fee: Long,
                           timestamp: Long,
                           networkByte: Byte,
                           signature: ByteStr)
  extends SignedTransaction {
  override val transactionType: TransactionType.Value = TransactionType.DataTransaction
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  val toSign: Coeval[Array[Byte]] = Coeval.evalOnce( Bytes.concat(Array(transactionType.id.toByte, networkByte),
    sender.publicKey,
    BytesSerializable.arrayWithSize(data),
    Longs.toByteArray(fee),
    Longs.toByteArray(timestamp)))


  override val json: Coeval[JsObject] = Coeval.evalOnce(jsonBase() ++ Json.obj(
    "data" -> Base58.encode(data),
    "fee" -> fee,
    "networkByte" -> networkByte))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(toSign(), signature.arr))

}

object DataTransaction {
  val MaxDataSize = 140

  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    val networkByte = bytes.head
    val body = bytes.tail
    val sender = PublicKeyAccount(body.slice(0, KeyLength))
    val (data, dataLength: Int) = Deser.parseArraySize(body, KeyLength)
    val fee = Longs.fromByteArray(body.slice(dataLength, dataLength + 8))
    val timestamp = Longs.fromByteArray(body.slice(dataLength + 8, dataLength + 16))
    val signature = body.slice(dataLength + 16, dataLength + 16 + SignatureLength)

    DataTransaction.create(sender, data, fee, timestamp, networkByte, ByteStr(signature))
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def create(sender: PublicKeyAccount,
                     data: Array[Byte],
                     fee: Long,
                     timestamp: Long,
                     networkByte: Byte,
                     signature: ByteStr) =

    if (data.length > MaxDataSize) {
      Left(ValidationError.TooBigArray)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(DataTransaction(sender, data, fee, timestamp, networkByte, signature))
    }

  def create(sender: PrivateKeyAccount,
             data: Array[Byte],
             fee: Long,
             networkByte: Byte,
             timestamp: Long): Either[ValidationError, DataTransaction] =
    create(sender, data, fee, timestamp, networkByte, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(EllipticCurveImpl.sign(sender.privateKey, unverified.toSign())))
    }

}
