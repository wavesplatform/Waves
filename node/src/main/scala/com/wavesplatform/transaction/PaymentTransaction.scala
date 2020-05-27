package com.wavesplatform.transaction

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.serialization.impl.PaymentTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.PaymentTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class PaymentTransaction(
    sender: PublicKey,
    recipient: Address,
    amount: Long,
    fee: Long,
    timestamp: Long,
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
}
