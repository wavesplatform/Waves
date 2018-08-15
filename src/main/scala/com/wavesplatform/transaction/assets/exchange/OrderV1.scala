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
case class OrderV1(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                   @ApiModelProperty(dataType = "java.lang.String", example = "") matcherPublicKey: PublicKeyAccount,
                   assetPair: AssetPair,
                   @ApiModelProperty(dataType = "java.lang.String", example = "buy") orderType: OrderType,
                   @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8", example = "100000000") price: Long,
                   @ApiModelProperty("Amount in AssetPair.second") amount: Long,
                   @ApiModelProperty(value = "Creation timestamp") timestamp: Long,
                   @ApiModelProperty(value = "Order time to live, max = 30 days") expiration: Long,
                   @ApiModelProperty(example = "100000") matcherFee: Long,
                   @ApiModelProperty(dataType = "Proofs") proofs: Proofs)
    extends Order
    with Signed {

  override def version: Byte = 1

  override def signature: Array[Byte] = proofs.proofs(0).arr

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    senderPublicKey.publicKey ++ matcherPublicKey.publicKey ++
      assetPair.bytes ++ orderType.bytes ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
      Longs.toByteArray(matcherFee)
  )

  val signatureValid = Coeval.evalOnce(crypto.verify(signature, bodyBytes(), senderPublicKey.publicKey))

  @ApiModelProperty(hidden = true)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ signature)
}

object OrderV1 {
  private val AssetIdLength = 32

  def apply(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
            @ApiModelProperty(dataType = "java.lang.String", example = "") matcherPublicKey: PublicKeyAccount,
            assetPair: AssetPair,
            @ApiModelProperty(dataType = "java.lang.String", example = "buy") orderType: OrderType,
            @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8", example = "100000000") price: Long,
            @ApiModelProperty("Amount in AssetPair.second") amount: Long,
            @ApiModelProperty(value = "Creation timestamp") timestamp: Long,
            @ApiModelProperty(value = "Order time to live, max = 30 days") expiration: Long,
            @ApiModelProperty(example = "100000") matcherFee: Long,
            @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]): OrderV1 = {
    OrderV1(senderPublicKey,
            matcherPublicKey,
            assetPair,
            orderType,
            price,
            amount,
            timestamp,
            expiration,
            matcherFee,
            Proofs(Seq(ByteStr(signature))))
  }

  def buy(sender: PrivateKeyAccount,
          matcher: PublicKeyAccount,
          pair: AssetPair,
          price: Long,
          amount: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, OrderType.BUY, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
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
           matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, OrderType.SELL, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
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
            matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def parseBytes(bytes: Array[Byte]): Try[OrderV1] = Try {
    val readByte: State[Int, Byte] = State { from =>
      (from + 1, bytes(from))
    }
    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }
    def parse[T](f: (Array[Byte], Int, Int) => (T, Int), size: Int): State[Int, T] = State { from =>
      val (res, off) = f(bytes, from, size)
      (off, res)
    }
    val makeOrder = for {
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
      signature     <- read(identity, SignatureLength)
    } yield {
      OrderV1(
        sender,
        matcher,
        AssetPair(amountAssetId.map(ByteStr(_)), priceAssetId.map(ByteStr(_))),
        OrderType(orderType),
        price,
        amount,
        timestamp,
        expiration,
        matcherFee,
        signature
      )
    }
    makeOrder.run(0).value._2
  }
}
