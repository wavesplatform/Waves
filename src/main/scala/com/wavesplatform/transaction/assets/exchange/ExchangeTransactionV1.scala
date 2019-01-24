package com.wavesplatform.transaction.assets.exchange

import cats.data.State
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction._
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

case class ExchangeTransactionV1(buyOrder: OrderV1,
                                 sellOrder: OrderV1,
                                 amount: Long,
                                 price: Long,
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

  override val signedDescendants: Coeval[Seq[Signed]] = Coeval.evalOnce(Seq(buyOrder, sellOrder))
}

object ExchangeTransactionV1 extends TransactionParserFor[ExchangeTransactionV1] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = ExchangeTransaction.typeId

  def create(matcher: PrivateKeyAccount,
             buyOrder: OrderV1,
             sellOrder: OrderV1,
             amount: Long,
             price: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long): Either[ValidationError, TransactionT] = {
    create(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, ByteStr.empty).right.map { unverified =>
      unverified.copy(signature = ByteStr(crypto.sign(matcher.privateKey, unverified.bodyBytes())))
    }
  }

  def create(buyOrder: OrderV1,
             sellOrder: OrderV1,
             amount: Long,
             price: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long,
             signature: ByteStr): Either[ValidationError, TransactionT] = {
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
        create(o1, o2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, signature)
          .fold(left => Failure(new Exception(left.toString)), right => Success(right))
      }
      makeTransaction.run(0).value._2
    }.flatten
  }
}
