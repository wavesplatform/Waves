package com.wavesplatform.transaction.assets.exchange

import cats.implicits._
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.{PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction._
import com.wavesplatform.transaction.description._
import monix.eval.Coeval

import scala.util.Try

case class ExchangeTransactionV1(
    buyOrder: OrderV1,
    sellOrder: OrderV1,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long,
    signature: ByteStr
) extends ExchangeTransaction
    with SignedTransaction {

  override def version: Byte           = 1
  override val builder                 = ExchangeTransactionV1
  override val assetFee: (Asset, Long) = (Waves, fee)

  override val sender: PublicKey = buyOrder.matcherPublicKey

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Array(builder.typeId) ++
        Ints.toByteArray(buyOrder.bytes().length) ++ Ints.toByteArray(sellOrder.bytes().length) ++
        buyOrder.bytes() ++ sellOrder.bytes() ++ Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
        Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
        Longs.toByteArray(timestamp)
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ signature.arr)

  override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(Seq(buyOrder, sellOrder))
}

object ExchangeTransactionV1 extends TransactionParserFor[ExchangeTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = ExchangeTransaction.typeId

  def create(
      matcher: PrivateKey,
      buyOrder: OrderV1,
      sellOrder: OrderV1,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, TransactionT] = {
    create(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(matcher, unverified.bodyBytes())))
    }
  }

  def create(
      buyOrder: OrderV1,
      sellOrder: OrderV1,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long,
      signature: ByteStr
  ): Either[ValidationError, TransactionT] = {
    validateExchangeParams(
      buyOrder,
      sellOrder,
      amount,
      price,
      buyMatcherFee,
      sellMatcherFee,
      fee,
      timestamp
    ).map { _ =>
      ExchangeTransactionV1(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
    }
  }

  override def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      ExchangeTransaction
        .validateExchangeParams(tx)
        .map(_ => tx)
        .foldToTry
    }
  }

  val byteTailDescription: ByteEntity[ExchangeTransactionV1] = {
    (
      IntBytes(tailIndex(1), "Buy order object length (BN)"),
      IntBytes(tailIndex(2), "Sell order object length (SN)"),
      OrderV1Bytes(tailIndex(3), "Buy order object", "BN, see OrderV1 structure"),
      OrderV1Bytes(tailIndex(4), "Sell order object", "SN, see OrderV1 structure"),
      LongBytes(tailIndex(5), "Price"),
      LongBytes(tailIndex(6), "Amount"),
      LongBytes(tailIndex(7), "Buy matcher fee"),
      LongBytes(tailIndex(8), "Sell matcher fee"),
      LongBytes(tailIndex(9), "Fee"),
      LongBytes(tailIndex(10), "Timestamp"),
      SignatureBytes(tailIndex(11), "Signature")
    ) mapN {
      case (_, _, buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature) =>
        ExchangeTransactionV1(
          buyOrder = buyOrder,
          sellOrder = sellOrder,
          amount = amount,
          price = price,
          buyMatcherFee = buyMatcherFee,
          sellMatcherFee = sellMatcherFee,
          fee = fee,
          timestamp = timestamp,
          signature = signature
        )
    }
  }
}
