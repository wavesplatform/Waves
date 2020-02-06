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

case class PaymentTransaction(sender: PublicKey, recipient: Address, amount: Long, fee: Long, timestamp: Long, signature: ByteStr)
    extends SignedTransaction
    with TxWithFee.InWaves {

  override val builder             = PaymentTransaction
  override val id: Coeval[ByteStr] = Coeval.evalOnce(signature)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(builder.serializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(builder.serializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(builder.serializer.toJson(this))

  override def chainByte: ChainId = recipient.chainId
}

object PaymentTransaction extends TransactionParser {
  override val typeId: TxType                    = 2
  override val supportedVersions: Set[TxVersion] = Set(1)

  val serializer = PaymentTxSerializer

  override def parseBytes(bytes: Array[TxVersion]): Try[PaymentTransaction] =
    serializer.parseBytes(bytes)


  implicit val validator: TxValidator[PaymentTransaction] = PaymentTxValidator

  def selfSigned(sender: KeyPair, recipient: Address, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction] = {
    create(sender.publicKey, recipient, amount, fee, timestamp, ByteStr.empty).right.map(unsigned => {
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
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
    PaymentTransaction(sender, recipient, amount, fee, timestamp, signature).validatedEither
}
