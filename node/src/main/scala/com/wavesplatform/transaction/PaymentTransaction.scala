package com.wavesplatform.transaction

import cats.syntax.either._
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.serialization.impl.PaymentTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.PaymentTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class PaymentTransaction private (
    sender: PublicKey,
    recipient: Address,
    amount: TxAmount,
    fee: TxAmount,
    timestamp: TxTimestamp,
    signature: ByteStr,
    chainId: Byte
) extends SignedTransaction
    with TxWithFee.InWaves {

  override val builder             = PaymentTransaction
  override val id: Coeval[ByteStr] = Coeval.evalOnce(signature)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))
}

object PaymentTransaction extends TransactionParser {
  type TransactionT = PaymentTransaction

  override val typeId: TxType                    = 2: Byte
  override val supportedVersions: Set[TxVersion] = Set(1)

  val serializer = PaymentTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[PaymentTransaction] =
    serializer.parseBytes(bytes)

  implicit val validator: TxValidator[PaymentTransaction] = PaymentTxValidator

  def create(sender: KeyPair, recipient: Address, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] = {
    create(sender.publicKey, recipient, amount, fee, timestamp, ByteStr.empty).map(unsigned => {
      unsigned.copy(signature = crypto.sign(sender.privateKey, unsigned.bodyBytes()))
    })
  }

  def create(
      sender: PublicKey,
      recipient: Address,
      amount: Long,
      fee: Long,
      timestamp: Long,
      signature: ByteStr
  ): Either[ValidationError, PaymentTransaction] =
    for {
      fee <- TxAmount.from(fee).leftMap(_ => TxValidationError.InsufficientFee)
      amount <- TxAmount.from(amount).leftMap(_ => TxValidationError.NonPositiveAmount(amount, "waves"))
      tx <- PaymentTransaction(sender, recipient, amount, fee, timestamp, signature, recipient.chainId).validatedEither
    } yield tx
}
