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

case class ExchangeTransactionV2(
    buyOrder: Order,
    sellOrder: Order,
    amount: Long,
    price: Long,
    buyMatcherFee: Long,
    sellMatcherFee: Long,
    fee: Long,
    timestamp: Long,
    proofs: Proofs
) extends ExchangeTransaction {

  import ExchangeTransactionV2._

  override def version: Byte = 2

  override val builder                 = ExchangeTransactionV2
  override val assetFee: (Asset, Long) = (Waves, fee)

  override val sender: PublicKey = buyOrder.matcherPublicKey

  override val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Array(0: Byte, builder.typeId, version) ++
        Ints.toByteArray(buyOrder.bytes().length) ++ orderMark(buyOrder.version) ++ buyOrder.bytes() ++
        Ints.toByteArray(sellOrder.bytes().length) ++ orderMark(sellOrder.version) ++ sellOrder.bytes() ++
        Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
        Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
        Longs.toByteArray(timestamp)
    )

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ proofs.bytes())
}

object ExchangeTransactionV2 extends TransactionParserFor[ExchangeTransactionV2] with TransactionParser.MultipleVersions {

  private def orderMark(version: Byte): Array[Byte] = {
    if (version == 1) {
      Array(1: Byte)
    } else {
      Array()
    }
  }
  override val typeId: Byte                 = ExchangeTransaction.typeId
  override val supportedVersions: Set[Byte] = Set(2)

  def create(
      matcher: PrivateKey,
      buyOrder: Order,
      sellOrder: Order,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long
  ): Either[ValidationError, TransactionT] = {
    create(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs.empty).map { unverified =>
      unverified.copy(proofs = Proofs(List(ByteStr(crypto.sign(matcher, unverified.bodyBytes())))))
    }
  }

  def create(
      buyOrder: Order,
      sellOrder: Order,
      amount: Long,
      price: Long,
      buyMatcherFee: Long,
      sellMatcherFee: Long,
      fee: Long,
      timestamp: Long,
      proofs: Proofs
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
      ExchangeTransactionV2(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs)
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

  val byteTailDescription: ByteEntity[ExchangeTransactionV2] = {
    (
      OrderBytes(tailIndex(1), "Buy order"),
      OrderBytes(tailIndex(2), "Sell order"),
      LongBytes(tailIndex(3), "Price"),
      LongBytes(tailIndex(4), "Amount"),
      LongBytes(tailIndex(5), "Buy matcher fee"),
      LongBytes(tailIndex(6), "Sell matcher fee"),
      LongBytes(tailIndex(7), "Fee"),
      LongBytes(tailIndex(8), "Timestamp"),
      ProofsBytes(tailIndex(9))
    ) mapN {
      case (buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs) =>
        ExchangeTransactionV2(
          buyOrder = buyOrder,
          sellOrder = sellOrder,
          amount = amount,
          price = price,
          buyMatcherFee = buyMatcherFee,
          sellMatcherFee = sellMatcherFee,
          fee = fee,
          timestamp = timestamp,
          proofs = proofs
        )
    }
  }
}
