package com.wavesplatform.transaction.assets.exchange

import cats.data.State
import com.google.common.primitives.Longs
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.crypto._
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import monix.eval.Coeval

import scala.util.Try

/**
  * Order to matcher service for asset exchange
  */
@ApiModel
case class OrderV1(@ApiModelProperty(
                     value = "Base58 encoded Sender public key",
                     required = true,
                     dataType = "string",
                     example = "HBqhfdFASRQ5eBBpu2y6c6KKi1az6bMx8v1JxX4iW1Q8"
                   )
                   senderPublicKey: PublicKeyAccount,
                   @ApiModelProperty(
                     value = "Base58 encoded Matcher public key",
                     required = true,
                     dataType = "string",
                     example = "HBqhfdFASRQ5eBBpu2y6c6KKi1az6bMx8v1JxX4iW1Q8"
                   )
                   matcherPublicKey: PublicKeyAccount,
                   @ApiModelProperty(value = "Asset pair", required = true)
                   assetPair: AssetPair,
                   @ApiModelProperty(
                     value = "Order type (sell or buy)",
                     required = true,
                     dataType = "string",
                     example = "sell"
                   )
                   orderType: OrderType,
                   @ApiModelProperty(value = "Amount", required = true)
                   amount: Long,
                   @ApiModelProperty(value = "Price", required = true)
                   price: Long,
                   @ApiModelProperty(value = "Timestamp", required = true)
                   timestamp: Long,
                   @ApiModelProperty(value = "Expiration timestamp", required = true)
                   expiration: Long,
                   @ApiModelProperty(value = "Matcher fee", required = true)
                   matcherFee: Long,
                   @ApiModelProperty(
                     value = "Order proofs",
                     required = true,
                     dataType = "[Ljava.lang.String;"
                   )
                   proofs: Proofs)
    extends Order
    with Signed {

  @ApiModelProperty(required = true, dataType = "long", example = "1")
  override def version: Byte = 1

  @ApiModelProperty(hidden = true)
  override def signature: Array[Byte] = proofs.proofs(0).arr

  @ApiModelProperty(hidden = true)
  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    senderPublicKey.publicKey ++ matcherPublicKey.publicKey ++
      assetPair.bytes ++ orderType.bytes ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
      Longs.toByteArray(matcherFee)
  )

  @ApiModelProperty(hidden = true)
  val signatureValid = Coeval.evalOnce(crypto.verify(signature, bodyBytes(), senderPublicKey.publicKey))

  @ApiModelProperty(hidden = true)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ signature)
}

object OrderV1 {
  private val AssetIdLength = 32

  def apply(senderPublicKey: PublicKeyAccount,
            matcherPublicKey: PublicKeyAccount,
            assetPair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            signature: Array[Byte]): OrderV1 = {
    OrderV1(senderPublicKey,
            matcherPublicKey,
            assetPair,
            orderType,
            amount,
            price,
            timestamp,
            expiration,
            matcherFee,
            Proofs(List(ByteStr(signature))))
  }

  def buy(sender: PrivateKeyAccount,
          matcher: PublicKeyAccount,
          pair: AssetPair,
          amount: Long,
          price: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(List(ByteStr(sig))))
  }

  def sell(sender: PrivateKeyAccount,
           matcher: PublicKeyAccount,
           pair: AssetPair,
           amount: Long,
           price: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(List(ByteStr(sig))))
  }

  def apply(sender: PrivateKeyAccount,
            matcher: PublicKeyAccount,
            pair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long): OrderV1 = {
    val unsigned = OrderV1(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.copy(proofs = Proofs(List(ByteStr(sig))))
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
      sender  <- read(PublicKeyAccount.apply, KeyLength)
      matcher <- read(PublicKeyAccount.apply, KeyLength)
      amountAssetId <- parse(Deser.parseByteArrayOption, AssetIdLength)
        .map {
          case Some(arr) => IssuedAsset(ByteStr(arr))
          case None      => Waves
        }
      priceAssetId <- parse(Deser.parseByteArrayOption, AssetIdLength)
        .map {
          case Some(arr) => IssuedAsset(ByteStr(arr))
          case None      => Waves
        }
      orderType  <- readByte
      price      <- read(Longs.fromByteArray _, 8)
      amount     <- read(Longs.fromByteArray _, 8)
      timestamp  <- read(Longs.fromByteArray _, 8)
      expiration <- read(Longs.fromByteArray _, 8)
      matcherFee <- read(Longs.fromByteArray _, 8)
      signature  <- read(identity, SignatureLength)
    } yield {
      OrderV1(
        sender,
        matcher,
        AssetPair(amountAssetId, priceAssetId),
        OrderType(orderType),
        amount,
        price,
        timestamp,
        expiration,
        matcherFee,
        signature
      )
    }
    makeOrder.run(0).value._2
  }
}
