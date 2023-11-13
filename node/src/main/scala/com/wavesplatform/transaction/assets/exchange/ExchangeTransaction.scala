package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{AddressScheme, PrivateKey, PublicKey}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.serialization.impl.ExchangeTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.validation.impl.ExchangeTxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

case class ExchangeTransaction(
    version: TxVersion,
    order1: Order,
    order2: Order,
    amount: TxExchangeAmount,
    price: TxExchangePrice,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: TxPositiveAmount,
    timestamp: Long,
    proofs: Proofs,
    chainId: Byte
) extends Transaction(TransactionType.Exchange, order1.assetPair.checkedAssets)
    with VersionedTransaction.ToV3
    with ProvenTransaction
    with TxWithFee.InWaves
    with FastHashId
    with SigProofsSwitch
    with PBSince.V3 {

  val (buyOrder, sellOrder) = if (order1.orderType == OrderType.BUY) (order1, order2) else (order2, order1)

  override protected def verifyFirstProof(isRideV6Activated: Boolean): Either[GenericError, Unit] =
    super.verifyFirstProof(isRideV6Activated).tap { _ =>
      if (isRideV6Activated) {
        order1.firstProofIsValidSignatureAfterV6
        order2.firstProofIsValidSignatureAfterV6
      } else {
        order1.firstProofIsValidSignatureBeforeV6
        order2.firstProofIsValidSignatureBeforeV6
      }
    }

  override val sender: PublicKey = buyOrder.matcherPublicKey

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(ExchangeTxSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(ExchangeTxSerializer.toBytes(this))
  override val json: Coeval[JsObject]         = Coeval.evalOnce(ExchangeTxSerializer.toJson(this))
}

object ExchangeTransaction extends TransactionParser {
  type TransactionT = ExchangeTransaction

  implicit val validator: TxValidator[ExchangeTransaction] = ExchangeTxValidator

  implicit def sign(tx: ExchangeTransaction, privateKey: PrivateKey): ExchangeTransaction =
    tx.copy(proofs = Proofs(crypto.sign(privateKey, tx.bodyBytes())))

  override def parseBytes(bytes: Array[TxVersion]): Try[ExchangeTransaction] =
    ExchangeTxSerializer.parseBytes(bytes)

  val typeId: TxType = 7: Byte

  def create(
      version: TxVersion,
      order1: Order,
      order2: Order,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long,
      proofs: Proofs = Proofs.empty,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, ExchangeTransaction] =
    for {
      fee    <- TxPositiveAmount(fee)(TxValidationError.InsufficientFee)
      amount <- TxExchangeAmount(amount)(GenericError(TxExchangeAmount.errMsg))
      price  <- TxExchangePrice(price)(GenericError(TxExchangePrice.errMsg))
      tx <- ExchangeTransaction(
        version,
        order1,
        order2,
        amount,
        price,
        buyMatcherFee,
        sellMatcherFee,
        fee,
        timestamp,
        proofs,
        chainId
      ).validatedEither
    } yield tx

  def signed(
      version: TxVersion,
      matcher: PrivateKey,
      order1: Order,
      order2: Order,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long,
      chainId: Byte = AddressScheme.current.chainId
  ): Either[ValidationError, ExchangeTransaction] =
    create(version, order1, order2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs.empty, chainId).map(_.signWith(matcher))
}
