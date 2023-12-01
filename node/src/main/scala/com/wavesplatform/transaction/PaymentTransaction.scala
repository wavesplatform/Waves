package com.wavesplatform.transaction

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

case class PaymentTransaction(
    sender: PublicKey,
    recipient: Address,
    amount: TxPositiveAmount,
    fee: TxPositiveAmount,
    timestamp: TxTimestamp,
    signature: ByteStr,
    chainId: Byte
) extends Transaction(TransactionType.Payment)
    with ProvenTransaction
    with TxWithFee.InWaves {

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PaymentTxSerializer.bodyBytes(this))

  def proofs: Proofs = Proofs(signature)

  override val id: Coeval[ByteStr] = Coeval.evalOnce(signature)

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(PaymentTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]     = Coeval.evalOnce(PaymentTxSerializer.toJson(this))
}

object PaymentTransaction extends TransactionParser {
  type TransactionT = PaymentTransaction

  override val typeId: TxType = 2: Byte

  override def parseBytes(bytes: Array[TxVersion]): Try[PaymentTransaction] =
    PaymentTxSerializer.parseBytes(bytes)

  implicit val validator: TxValidator[PaymentTransaction] = PaymentTxValidator

  def create(sender: KeyPair, recipient: Address, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] =
    create(sender.publicKey, recipient, amount, fee, timestamp, ByteStr.empty).map(unsigned => {
      unsigned.copy(signature = crypto.sign(sender.privateKey, unsigned.bodyBytes()))
    })

  def create(
      sender: PublicKey,
      recipient: Address,
      amount: Long,
      fee: Long,
      timestamp: Long,
      signature: ByteStr
  ): Either[ValidationError, PaymentTransaction] =
    for {
      fee    <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      amount <- TxPositiveAmount(amount)(TxValidationError.NonPositiveAmount(amount, "waves"))
      tx     <- PaymentTransaction(sender, recipient, amount, fee, timestamp, signature, recipient.chainId).validatedEither
    } yield tx
}
