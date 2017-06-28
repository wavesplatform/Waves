package scorex.transaction.Data

import com.google.common.primitives.{Bytes, Longs}
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
sealed trait DataTransaction extends SignedTransaction {
  def data: Array[Byte]
  def fee: Long
  def networkByte: Byte

}

object DataTransaction {

  private case class DataTransactionImpl(sender: PublicKeyAccount,
                                         data: Array[Byte],
                                         fee: Long,
                                         timestamp: Long,
                                         networkByte: Byte,
                                         signature: Array[Byte])
    extends DataTransaction {
    override val transactionType: TransactionType.Value = TransactionType.DataTransaction
    override val assetFee: (Option[AssetId], Long) = (None, fee)

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte,networkByte),
      sender.publicKey,
      BytesSerializable.arrayWithSize(data),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp))


    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "data" -> Base58.encode(data),
      "fee" -> fee,
      "networkByte" -> networkByte
    )

    override lazy val bytes: Array[Byte] = Bytes.concat(toSign, signature)

  }

  val MaxDataSize = 140

  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    val networkByte   = bytes.head
    val body = bytes.tail
    val sender = PublicKeyAccount(body.slice(0, KeyLength))
    val (data, dataLength: Int) = Deser.parseArraySize(body, KeyLength)
    val fee = Longs.fromByteArray(body.slice(dataLength, dataLength + 8))
    val timestamp = Longs.fromByteArray(body.slice(dataLength + 8, dataLength + 16))
    val signature = body.slice(dataLength + 16, dataLength + 16 + SignatureLength)

    DataTransaction.create(sender, data, fee, timestamp, networkByte, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               data: Array[Byte],
                               fee: Long,
                               timestamp: Long,
                               networkByte: Byte,
                               signature: Option[Array[Byte]] = None) =

    if (data.length > MaxDataSize) {
      Left(ValidationError.TooBigArray)
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(DataTransactionImpl(sender, data, fee, timestamp, networkByte, signature.orNull))
    }


  def create(sender: PublicKeyAccount,
             data: Array[Byte],
             fee: Long,
             timestamp: Long,
             networkByte: Byte,
             signature: Array[Byte]): Either[ValidationError, DataTransaction] =
    createUnverified(sender, data, fee, timestamp, networkByte, Some(signature)).right.flatMap(SignedTransaction.verify)

  def create(sender: PrivateKeyAccount,
             data: Array[Byte],
             fee: Long,
             networkByte: Byte,
             timestamp: Long): Either[ValidationError, DataTransaction] =
    createUnverified(sender, data, fee, timestamp, networkByte).right.map { unverified =>
      unverified.copy(signature = EllipticCurveImpl.sign(sender, unverified.toSign))
    }

}
