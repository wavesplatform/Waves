package com.wavesplatform.transaction.assets.exchange

import cats.data.State
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction._
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

case class ExchangeTransactionV2(buyOrder: Order,
                                 sellOrder: Order,
                                 amount: Long,
                                 price: Long,
                                 buyMatcherFee: Long,
                                 sellMatcherFee: Long,
                                 fee: Long,
                                 timestamp: Long,
                                 proofs: Proofs)
    extends ExchangeTransaction {
  import ExchangeTransactionV2._
  override def version: Byte                     = 2
  override val builder                           = ExchangeTransactionV2
  override val assetFee: (Option[AssetId], Long) = (None, fee)

  @ApiModelProperty(hidden = true)
  override val sender: PublicKeyAccount = buyOrder.matcherPublicKey

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Array(0: Byte, builder.typeId, version) ++
      Ints.toByteArray(buyOrder.bytes().length) ++ orderMark(buyOrder.version) ++ buyOrder.bytes() ++
      Ints.toByteArray(sellOrder.bytes().length) ++ orderMark(sellOrder.version) ++ sellOrder.bytes() ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(buyMatcherFee) ++ Longs.toByteArray(sellMatcherFee) ++ Longs.toByteArray(fee) ++
      Longs.toByteArray(timestamp))

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

  def create(matcher: PrivateKeyAccount,
             buyOrder: Order,
             sellOrder: Order,
             amount: Long,
             price: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long): Either[ValidationError, TransactionT] = {
    create(buyOrder, sellOrder, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, Proofs.empty).map { unverified =>
      unverified.copy(proofs = Proofs(Seq(ByteStr(crypto.sign(matcher.privateKey, unverified.bodyBytes())))))
    }
  }

  def create(buyOrder: Order,
             sellOrder: Order,
             amount: Long,
             price: Long,
             buyMatcherFee: Long,
             sellMatcherFee: Long,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
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

  override def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] = {
    def back(off: Int): State[Int, Unit] = State { from =>
      (from - off, ())
    }
    val readByte: State[Int, Byte] = State { from =>
      (from + 1, bytes(from))
    }
    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }
    def readEnd[T](f: Array[Byte] => T): State[Int, T] = State { from =>
      (from, f(bytes.drop(from)))
    }

    Try {
      val makeTransaction = for {
        o1Size         <- read(Ints.fromByteArray _, 4)
        o1Ver          <- readByte
        _              <- back(if (o1Ver != 1) { 1 } else { 0 })
        o1             <- read(if (o1Ver == 1) { OrderV1.parseBytes _ } else { OrderV2.parseBytes _ }, o1Size).map(_.get)
        o2Size         <- read(Ints.fromByteArray _, 4)
        o2Ver          <- readByte
        _              <- back(if (o2Ver != 1) { 1 } else { 0 })
        o2             <- read(if (o2Ver == 1) { OrderV1.parseBytes _ } else { OrderV2.parseBytes _ }, o2Size).map(_.get)
        price          <- read(Longs.fromByteArray _, 8)
        amount         <- read(Longs.fromByteArray _, 8)
        buyMatcherFee  <- read(Longs.fromByteArray _, 8)
        sellMatcherFee <- read(Longs.fromByteArray _, 8)
        fee            <- read(Longs.fromByteArray _, 8)
        timestamp      <- read(Longs.fromByteArray _, 8)
        proofs         <- readEnd(Proofs.fromBytes)
      } yield {
        create(o1, o2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, proofs.right.get)
          .fold(left => Failure(new Exception(left.toString)), right => Success(right))
      }
      makeTransaction.run(0).value._2
    }.flatten
  }
}
