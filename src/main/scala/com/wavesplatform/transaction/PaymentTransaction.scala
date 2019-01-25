package com.wavesplatform.transaction

import java.util

import com.google.common.primitives.{Bytes, Ints, Longs}
import com.wavesplatform.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.TransactionParsers._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

case class PaymentTransaction private (sender: PublicKeyAccount, recipient: Address, amount: Long, fee: Long, timestamp: Long, signature: ByteStr)
    extends SignedTransaction {
  override val builder: TransactionParser        = PaymentTransaction
  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val id: Coeval[AssetId]               = Coeval.evalOnce(signature)

  override val json: Coeval[JsObject] = Coeval.evalOnce(jsonBase() ++ Json.obj("recipient" -> recipient.address, "amount" -> amount))

  private val hashBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(Array(builder.typeId),
                 Longs.toByteArray(timestamp),
                 sender.publicKey,
                 recipient.bytes.arr,
                 Longs.toByteArray(amount),
                 Longs.toByteArray(fee)))

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(Ints.toByteArray(builder.typeId),
                 Longs.toByteArray(timestamp),
                 sender.publicKey,
                 recipient.bytes.arr,
                 Longs.toByteArray(amount),
                 Longs.toByteArray(fee)))

  val hash: Coeval[Array[Byte]] = Coeval.evalOnce(crypto.fastHash(hashBytes()))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(hashBytes(), signature.arr))

}

object PaymentTransaction extends TransactionParserFor[PaymentTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 2

  val RecipientLength: Int = Address.AddressLength

  private val SenderLength = 32
  private val FeeLength    = 8
  private val BaseLength   = TimestampLength + SenderLength + RecipientLength + AmountLength + FeeLength + SignatureLength

  def create(sender: PrivateKeyAccount, recipient: Address, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    create(sender, recipient, amount, fee, timestamp, ByteStr.empty).right.map(unsigned => {
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    })
  }

  def create(sender: PublicKeyAccount,
             recipient: Address,
             amount: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    if (amount <= 0) {
      Left(ValidationError.NegativeAmount(amount, "waves")) //CHECK IF AMOUNT IS POSITIVE
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee()) //CHECK IF FEE IS POSITIVE
    } else if (Try(Math.addExact(amount, fee)).isFailure) {
      Left(ValidationError.OverflowError) // CHECK THAT fee+amount won't overflow Long
    } else {
      Right(PaymentTransaction(sender, recipient, amount, fee, timestamp, signature))
    }
  }

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      require(bytes.length >= BaseLength, "Data does not match base length")

      var position = 0

      //READ TIMESTAMP
      val timestampBytes = bytes.take(TimestampLength)
      val timestamp      = Longs.fromByteArray(timestampBytes)
      position += TimestampLength

      //READ SENDER
      val senderBytes = util.Arrays.copyOfRange(bytes, position, position + SenderLength)
      val sender      = PublicKeyAccount(senderBytes)
      position += SenderLength

      //READ RECIPIENT
      val recipientBytes = util.Arrays.copyOfRange(bytes, position, position + RecipientLength)
      val recipient      = Address.fromBytes(recipientBytes).explicitGet()
      position += RecipientLength

      //READ AMOUNT
      val amountBytes = util.Arrays.copyOfRange(bytes, position, position + AmountLength)
      val amount      = Longs.fromByteArray(amountBytes)
      position += AmountLength

      //READ FEE
      val feeBytes = util.Arrays.copyOfRange(bytes, position, position + FeeLength)
      val fee      = Longs.fromByteArray(feeBytes)
      position += FeeLength

      //READ SIGNATURE
      val signatureBytes = util.Arrays.copyOfRange(bytes, position, position + SignatureLength)

      PaymentTransaction
        .create(sender, recipient, amount, fee, timestamp, ByteStr(signatureBytes))
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

}
