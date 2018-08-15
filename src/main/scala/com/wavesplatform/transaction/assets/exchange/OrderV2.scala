package com.wavesplatform.transaction.assets.exchange

import com.google.common.primitives.Longs
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction._
import com.wavesplatform.crypto._
import scala.util.Try
import cats.data.State

/**
  * Order to matcher service for asset exchange
  */
case class OrderV2(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                   @ApiModelProperty(dataType = "java.lang.String", example = "") matcherPublicKey: PublicKeyAccount,
                   assetPair: AssetPair,
                   @ApiModelProperty(dataType = "java.lang.String", example = "buy") orderType: OrderType,
                   @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8", example = "100000000") price: Long,
                   @ApiModelProperty("Amount in AssetPair.second") amount: Long,
                   @ApiModelProperty(value = "Creation timestamp") timestamp: Long,
                   @ApiModelProperty(value = "Order time to live, max = 30 days") expiration: Long,
                   @ApiModelProperty(example = "100000") matcherFee: Long,
                   @ApiModelProperty(dataType = "Proofs") proofs: Proofs)
    extends Order {

  override def version: Byte = 2

  override def signature: Array[Byte] = proofs.proofs(0).arr

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    (version +: senderPublicKey.publicKey) ++ matcherPublicKey.publicKey ++
      assetPair.bytes ++ orderType.bytes ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
      Longs.toByteArray(matcherFee)
  )

  @ApiModelProperty(hidden = true)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ proofs.bytes())
}

object OrderV2 {
  private val AssetIdLength = 32

  def buy(sender: PrivateKeyAccount,
          matcher: PublicKeyAccount,
          pair: AssetPair,
          price: Long,
          amount: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long): Order = {
    val unsigned = OrderV2(sender, matcher, pair, OrderType.BUY, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def sell(sender: PrivateKeyAccount,
           matcher: PublicKeyAccount,
           pair: AssetPair,
           price: Long,
           amount: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long): Order = {
    val unsigned = OrderV2(sender, matcher, pair, OrderType.SELL, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def apply(sender: PrivateKeyAccount,
            matcher: PublicKeyAccount,
            pair: AssetPair,
            orderType: OrderType,
            price: Long,
            amount: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long): Order = {
    val unsigned = OrderV2(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def parseBytes(bytes: Array[Byte]): Try[Order] = Try {
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
    def parse[T](f: (Array[Byte], Int, Int) => (T, Int), size: Int): State[Int, T] = State { from =>
      val (res, off) = f(bytes, from, size)
      (off, res)
    }
    val makeOrder = for {
      version <- readByte
      _ = if (version != 2) { throw new Exception(s"Incorrect order version: expect 2 but found $version") }
      sender        <- read(PublicKeyAccount.apply, KeyLength)
      matcher       <- read(PublicKeyAccount.apply, KeyLength)
      amountAssetId <- parse(Deser.parseByteArrayOption, AssetIdLength)
      priceAssetId  <- parse(Deser.parseByteArrayOption, AssetIdLength)
      orderType     <- readByte
      price         <- read(Longs.fromByteArray _, 8)
      amount        <- read(Longs.fromByteArray _, 8)
      timestamp     <- read(Longs.fromByteArray _, 8)
      expiration    <- read(Longs.fromByteArray _, 8)
      matcherFee    <- read(Longs.fromByteArray _, 8)
      maybeProofs   <- readEnd(Proofs.fromBytes)
    } yield {
      OrderV2(
        sender,
        matcher,
        AssetPair(amountAssetId.map(ByteStr(_)), priceAssetId.map(ByteStr(_))),
        OrderType(orderType),
        price,
        amount,
        timestamp,
        expiration,
        matcherFee,
        maybeProofs.right.get
      )
    }
    makeOrder.run(0).value._2
  }
}
