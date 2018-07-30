package com.wavesplatform.transaction.assets.exchange

import com.google.common.primitives.Longs
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction._
import scorex.crypto.signatures.Curve25519._
import scala.util.Try

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
    extends Order {

  override def version: Byte = 1

  override def signature: Array[Byte] = proofs.proofs(0).arr

  val signatureValid = Coeval.evalOnce(crypto.verify(signature, toSign, senderPublicKey.publicKey))

  @ApiModelProperty(hidden = true)
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(toSign ++ signature)

  override def updateProofs(p: Proofs): Order = copy(proofs = p)

  // For tests
  override def updateExpiration(nexpiration: Long): Order  = copy(expiration = nexpiration)
  override def updateTimestamp(ntimestamp: Long): Order    = copy(timestamp = ntimestamp)
  override def updateFee(fee: Long): Order                 = copy(matcherFee = fee)
  override def updateAmount(namount: Long): Order          = copy(amount = namount)
  override def updatePrice(nprice: Long): Order            = copy(price = nprice)
  override def updateMatcher(pk: PrivateKeyAccount): Order = copy(matcherPublicKey = pk)
  override def updateSender(pk: PrivateKeyAccount): Order  = copy(senderPublicKey = pk)
  override def updatePair(pair: AssetPair): Order          = copy(assetPair = pair)
  override def updateType(t: OrderType): Order             = copy(orderType = t)
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
            @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte]): Order = {
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
          matcherFee: Long): Order = {
    val unsigned = OrderV1(sender, matcher, pair, OrderType.BUY, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.toSign)
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
    val unsigned = OrderV1(sender, matcher, pair, OrderType.SELL, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.toSign)
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
    val unsigned = OrderV1(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.toSign)
    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def parseBytes(bytes: Array[Byte]): Try[Order] = Try {
    var from   = 0
    val sender = PublicKeyAccount(bytes.slice(from, from + KeyLength))
    from += KeyLength
    val matcher = PublicKeyAccount(bytes.slice(from, from + KeyLength))
    from += KeyLength
    val (amountAssetId, s0) = Deser.parseByteArrayOption(bytes, from, AssetIdLength)
    from = s0
    val (priceAssetId, s1) = Deser.parseByteArrayOption(bytes, from, AssetIdLength)
    from = s1
    val orderType = bytes(from)
    from += 1
    val price = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val amount = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val timestamp = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val expiration = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val matcherFee = Longs.fromByteArray(bytes.slice(from, from + 8))
    from += 8
    val signature = bytes.slice(from, from + SignatureLength)
    from += SignatureLength
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
}
