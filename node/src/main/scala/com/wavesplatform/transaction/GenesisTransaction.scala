package com.wavesplatform.transaction

import cats.implicits._
import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TransactionParsers._
import com.wavesplatform.transaction.description.{AddressBytes, ByteEntity, LongBytes}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class GenesisTransaction private (recipient: Address, amount: Long, timestamp: Long, signature: ByteStr) extends Transaction {

  import GenesisTransaction._

  override val builder: TransactionParser = GenesisTransaction
  override val assetFee: (Asset, Long)    = (Waves, 0)
  override val id: Coeval[ByteStr]        = Coeval.evalOnce(signature)

  override val json: Coeval[JsObject] = Coeval.evalOnce(
    Json.obj(
      "type"      -> builder.typeId,
      "id"        -> id().base58,
      "fee"       -> 0,
      "timestamp" -> timestamp,
      "signature" -> this.signature.base58,
      "recipient" -> recipient.stringRepr,
      "amount"    -> amount
    ))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val typeBytes      = Array(builder.typeId)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Bytes.ensureCapacity(Longs.toByteArray(amount), AmountLength, 0)
    val rcpBytes       = recipient.bytes.arr
    require(rcpBytes.length == Address.AddressLength)
    val res = Bytes.concat(typeBytes, timestampBytes, rcpBytes, amountBytes)
    require(res.length == TypeLength + BASE_LENGTH)
    res
  }

  override val bodyBytes: Coeval[Array[Byte]] = bytes
}

object GenesisTransaction extends TransactionParserFor[GenesisTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 1

  private val RECIPIENT_LENGTH = Address.AddressLength
  private val BASE_LENGTH      = TimestampLength + RECIPIENT_LENGTH + AmountLength

  def generateSignature(recipient: Address, amount: Long, timestamp: Long): Array[Byte] = {

    val typeBytes      = Bytes.ensureCapacity(Ints.toByteArray(typeId), TypeLength, 0)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), TimestampLength, 0)
    val amountBytes    = Longs.toByteArray(amount)
    val amountFill     = new Array[Byte](AmountLength - amountBytes.length)

    val data = Bytes.concat(typeBytes, timestampBytes, recipient.bytes.arr, Bytes.concat(amountFill, amountBytes))

    val h = crypto.fastHash(data)
    Bytes.concat(h, h)
  }

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {

      require(bytes.length >= BASE_LENGTH, "Data does not match base length")

      byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
        Either
          .cond(tx.amount >= 0, tx, TxValidationError.NegativeAmount(tx.amount, "waves"))
          .foldToTry
      }
    }.flatten
  }

  def create(recipient: Address, amount: Long, timestamp: Long): Either[ValidationError, GenesisTransaction] = {
    if (amount < 0) {
      Left(TxValidationError.NegativeAmount(amount, "waves"))
    } else {
      val signature = ByteStr(GenesisTransaction.generateSignature(recipient, amount, timestamp))
      Right(GenesisTransaction(recipient, amount, timestamp, signature))
    }
  }

  val byteTailDescription: ByteEntity[GenesisTransaction] = {
    (
      LongBytes(tailIndex(1), "Timestamp"),
      AddressBytes(tailIndex(2), "Recipient's address"),
      LongBytes(tailIndex(3), "Amount")
    ) mapN {
      case (timestamp, recipient, amount) =>
        GenesisTransaction(
          recipient = recipient,
          amount = amount,
          timestamp = timestamp,
          signature = ByteStr(generateSignature(recipient, amount, timestamp))
        )
    }
  }
}
