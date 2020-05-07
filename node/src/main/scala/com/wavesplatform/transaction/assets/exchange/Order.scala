package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.Validation.booleanOperators
import com.wavesplatform.transaction.serialization.impl.OrderSerializer
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject}

import scala.util.Try

/**
  * Order to matcher service for asset exchange
  */
case class Order(
    version: Order.Version,
    senderPublicKey: PublicKey,
    matcherPublicKey: PublicKey,
    assetPair: AssetPair,
    orderType: OrderType,
    amount: TxAmount,
    price: TxAmount,
    timestamp: TxTimestamp,
    expiration: TxTimestamp,
    matcherFee: TxAmount,
    matcherFeeAssetId: Asset = Waves,
    proofs: Proofs = Proofs.empty
) extends Proven {
  import Order._

  val sender: PublicKey = senderPublicKey

  def isValid(atTime: Long): Validation = {
    isValidAmount(amount, price) &&
    assetPair.isValid &&
    (matcherFee > 0) :| "matcherFee should be > 0" &&
    (matcherFee < MaxAmount) :| "matcherFee too large" &&
    (timestamp > 0) :| "timestamp should be > 0" &&
    (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
    (expiration >= atTime) :| "expiration should be > currentTime" &&
    (matcherFeeAssetId == Waves || version >= Order.V3) :| "matcherFeeAssetId should be waves"
  }

  def isValidAmount(matchAmount: Long, matchPrice: Long): Validation = {
    (matchAmount > 0) :| "amount should be > 0" &&
    (matchPrice > 0) :| "price should be > 0" &&
    (matchAmount < MaxAmount) :| "amount too large"
  }

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(OrderSerializer.bodyBytes(this))
  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
  val idStr: Coeval[String] = Coeval.evalOnce(id().toString)
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(OrderSerializer.toBytes(this))

  def getReceiveAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  def getSpendAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

  val json: Coeval[JsObject] = Coeval.evalOnce(OrderSerializer.toJson(this))

  override def toString: String = {
    val matcherFeeAssetIdStr = if (version == 3) s" matcherFeeAssetId=${matcherFeeAssetId.fold("Waves")(_.toString)}," else ""
    s"OrderV$version(id=${idStr()}, sender=$senderPublicKey, matcher=$matcherPublicKey, pair=$assetPair, tpe=$orderType, amount=$amount, " +
      s"price=$price, ts=$timestamp, exp=$expiration, fee=$matcherFee,$matcherFeeAssetIdStr proofs=$proofs)"
  }
}

object Order {
  type Id      = ByteStr
  type Version = Byte

  implicit lazy val jsonFormat: Format[Order] = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat

  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant     = 100000000L
  val MaxAmount: Long   = 100 * PriceConstant * PriceConstant

  val V1: Version = 1.toByte
  val V2: Version = 2.toByte
  val V3: Version = 3.toByte
  val V4: Version = 4.toByte

  implicit def sign(order: Order, privateKey: PrivateKey): Order =
    order.copy(proofs = Proofs(crypto.sign(privateKey, order.bodyBytes())))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      assetPair: AssetPair,
      orderType: OrderType,
      amount: TxAmount,
      price: TxAmount,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: TxAmount,
      matcherFeeAssetId: Asset = Asset.Waves
  ): Order =
    Order(version, sender.publicKey, matcher, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId).signWith(sender.privateKey)

  def buy(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: TxAmount,
      price: TxAmount,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: TxAmount,
      matcherFeeAssetId: Asset = Waves
  ): Order = {
    Order.selfSigned(version, sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
  }

  def sell(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: TxAmount,
      price: TxAmount,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: TxAmount,
      matcherFeeAssetId: Asset = Waves
  ): Order = {
    Order.selfSigned(version, sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
  }

  def parseBytes(version: Version, bytes: Array[Byte]): Try[Order] =
    OrderSerializer.parseBytes(version, bytes)
}
