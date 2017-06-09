package scorex.transaction.Data

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.EllipticCurveImpl.SignatureLength
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser}
import scorex.transaction._
import scorex.transaction.TransactionParser._

import scala.util.{Failure, Success, Try}
import scala.util.Try

/**
  * Created by DN on 30/05/2017.
  */
sealed trait DataTransaction extends SignedTransaction
{
  def data: Array[Byte]
  def fee: Long
  def dataLength: Long
}

object DataTransaction
{
  private case class  DataTransactionImpl(sender: PublicKeyAccount,
                                          data: Array[Byte],
                                          dataLength: Long,
                                          fee: Long,
                                          timestamp:Long,
                                          signature: Array[Byte])
  extends DataTransaction
  {
    override val transactionType: TransactionType.Value = TransactionType.DataTransaction
    override val assetFee: (Option[AssetId], Long)      = (None, fee)

    lazy val toSign: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte),
                                                sender.publicKey,
                                                BytesSerializable.arrayWithSize(data),
                                                Longs.toByteArray(dataLength),
                                                Longs.toByteArray(fee),
                                                Longs.toByteArray(timestamp))

    override lazy val json: JsObject = jsonBase() ++ Json.obj(
      "data" -> Base58.encode(data)
    )

    override lazy val bytes: Array[Byte] = Bytes.concat(Array(transactionType.id.toByte), signature, toSign)

  }

  val MaxDataSize = 140
  def parseTail(bytes: Array[Byte]): Try[DataTransaction] = Try {
    val signature = bytes.slice(0, SignatureLength)
    val txId      = bytes(SignatureLength)
    require(txId == TransactionType.DataTransaction.id.toByte, s"Signed tx id is not match")
    val sender                        = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
    val (dataLength, dataStart)       = Deser.parseArraySize(bytes, SignatureLength + KeyLength + 1)
    val data                          = Longs.fromByteArray(bytes.slice(dataStart, dataStart + 8))
    val fee                           = Longs.fromByteArray(bytes.slice(dataStart + 8, dataStart + 16))
    val timestamp                     = Longs.fromByteArray(bytes.slice(dataStart + 16, dataStart + 24))
    DataTransaction.create(sender,dataLength,data, fee, timestamp, signature)
      .fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  private def createUnverified(sender: PublicKeyAccount,
                               data: Array[Byte],
                               dataLength: Long,
                               fee: Long,
                               timestamp:Long,
                               signature: Option[Array[Byte]] = None) =

    if (dataLength > MaxDataSize) {
      Left(ValidationError.TooBigArray)
    }  else if (fee <= 0) {
      Left(ValidationError.InsufficientFee)
    } else {
      Right(DataTransactionImpl(sender, data, dataLength, fee, timestamp, signature.orNull))
    }


  def create(sender: PublicKeyAccount,
             data: Array[Byte],
             dataLength: Long,
             fee: Long,
             timestamp:Long,
             signature: Array[Byte]): Either[ValidationError, DataTransaction] =
    createUnverified(sender, data, dataLength , fee, timestamp, Some(signature))
      .right.flatMap(SignedTransaction.verify)

  def create(sender: PrivateKeyAccount,
             data: Array[Byte],
             dataLength: Long,
             fee: Long,
             timestamp: Long): Either[ValidationError, DataTransaction] =
    createUnverified(sender, data, dataLength , fee, timestamp).right.map { unverified =>
      unverified.copy(signature = EllipticCurveImpl.sign(sender, unverified.toSign))
    }

}
