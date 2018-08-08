package com.wavesplatform.transaction.assets.exchange

import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.transaction.ValidationError.{GenericError, OrderValidationError}
import com.wavesplatform.transaction._
import scorex.crypto.signatures.Curve25519._
import scala.util.{Failure, Success, Try}
import cats.data.State

case class ExchangeTransactionV1(buyOrder: Order,
                                 sellOrder: Order,
                                 price: Long,
                                 amount: Long,
                                 buyMatcherFee: Long,
                                 sellMatcherFee: Long,
                                 fee: Long,
                                 timestamp: Long,
                                 signature: ByteStr)
    extends ExchangeTransaction
    with SignedTransaction {

  override def version: Byte                     = 1
  override val builder                           = ExchangeTransactionV1
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  @ApiModelProperty(hidden = true)
  override val sender: PublicKeyAccount = buyOrder.matcherPublicKey

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Array(builder.typeId) ++
      Ints.toByteArray(buyOrder.bytes().length) ++ Ints.toByteArray(sellOrder.bytes().length) ++
      buyOrder.bytes() ++ sellOrder.bytes() ++ Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ signature.arr)

  override val signedDescendants: Coeval[Seq[Order]] = Coeval.evalOnce(Seq(buyOrder, sellOrder))
}

object ExchangeTransactionV1 extends TransactionParserFor[ExchangeTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = ExchangeTransaction.typeId

  def create(matcher: PrivateKeyAccount,
             buyOrder: OrderV1,
             sellOrder: OrderV1,
             price: Long,
             amount: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long): Either[ValidationError, TransactionT] = {
    create(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(matcher.privateKey, unverified.bodyBytes())))
    }
  }

  def create(buyOrder: OrderV1,
             sellOrder: OrderV1,
             price: Long,
             amount: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    lazy val priceIsValid: Boolean = price <= buyOrder.price && price >= sellOrder.price

    if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else if (amount <= 0) {
      Left(ValidationError.NegativeAmount(amount, "assets"))
    } else if (price <= 0) {
      Left(GenericError("price should be > 0"))
    } else if (price > Order.MaxAmount) {
      Left(GenericError("price too large"))
    } else if (amount > Order.MaxAmount) {
      Left(GenericError("amount too large"))
    } else if (sellMatcherFee > Order.MaxAmount) {
      Left(GenericError("sellMatcherFee too large"))
    } else if (buyMatcherFee > Order.MaxAmount) {
      Left(GenericError("buyMatcherFee too large"))
    } else if (fee > Order.MaxAmount) {
      Left(GenericError("fee too large"))
    } else if (buyOrder.orderType != OrderType.BUY) {
      Left(GenericError("buyOrder should has OrderType.BUY"))
    } else if (sellOrder.orderType != OrderType.SELL) {
      Left(GenericError("sellOrder should has OrderType.SELL"))
    } else if (buyOrder.matcherPublicKey != sellOrder.matcherPublicKey) {
      Left(GenericError("buyOrder.matcher should be the same as sellOrder.matcher"))
    } else if (buyOrder.assetPair != sellOrder.assetPair) {
      Left(GenericError("Both orders should have same AssetPair"))
    } else if (!buyOrder.isValid(timestamp)) {
      Left(OrderValidationError(buyOrder, buyOrder.isValid(timestamp).messages()))
    } else if (!sellOrder.isValid(timestamp)) {
      Left(OrderValidationError(sellOrder, sellOrder.isValid(timestamp).labels.mkString("\n")))
    } else if (!priceIsValid) {
      Left(GenericError("priceIsValid"))
    } else {
      Right(ExchangeTransactionV1(buyOrder, sellOrder, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature))
    }
  }

  override def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] = {
    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }

    Try {
      val makeTransaction = for {
        o1Size         <- read(Ints.fromByteArray _, 4)
        o2Size         <- read(Ints.fromByteArray _, 4)
        o1             <- read(OrderV1.parseBytes _, o1Size).map(_.get)
        o2             <- read(OrderV1.parseBytes _, o2Size).map(_.get)
        price          <- read(Longs.fromByteArray _, 8)
        amount         <- read(Longs.fromByteArray _, 8)
        buyMatcherFee  <- read(Longs.fromByteArray _, 8)
        sellMatcherFee <- read(Longs.fromByteArray _, 8)
        fee            <- read(Longs.fromByteArray _, 8)
        timestamp      <- read(Longs.fromByteArray _, 8)
        signature      <- read(ByteStr.apply, SignatureLength)
      } yield {
        create(o1, o2, price, amount, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
          .fold(left => Failure(new Exception(left.toString)), right => Success(right))
      }
      makeTransaction.run(0).value._2
    }.flatten
  }
}
