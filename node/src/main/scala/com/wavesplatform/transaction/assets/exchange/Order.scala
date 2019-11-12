package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.Validation.booleanOperators
import com.wavesplatform.transaction.serialization.impl.OrderSerializer
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

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
    amount: Long,
    price: Long,
    timestamp: Long,
    expiration: Long,
    matcherFee: Long,
    matcherFeeAssetId: Asset = Waves,
    proofs: Proofs = Proofs.empty
) extends Proven {
  import Order._

  @ApiModelProperty(hidden = true)
  val sender = senderPublicKey

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

  //  @ApiModelProperty(hidden = true)
  def isValidAmount(matchAmount: Long, matchPrice: Long): Validation = {
    (matchAmount > 0) :| "amount should be > 0" &&
    (matchPrice > 0) :| "price should be > 0" &&
    (matchAmount < MaxAmount) :| "amount too large" &&
    getSpendAmount(matchAmount, matchPrice).isRight :| "SpendAmount too large" &&
    (getSpendAmount(matchAmount, matchPrice).getOrElse(0L) > 0) :| "SpendAmount should be > 0" &&
    getReceiveAmount(matchAmount, matchPrice).isRight :| "ReceiveAmount too large" &&
    (getReceiveAmount(matchAmount, matchPrice).getOrElse(0L) > 0) :| "ReceiveAmount should be > 0"
  }

  @ApiModelProperty(hidden = true)
  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(OrderSerializer.bodyBytes(this))
  @ApiModelProperty(hidden = true)
  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
  @ApiModelProperty(hidden = true)
  val idStr: Coeval[String] = Coeval.evalOnce(id().toString)
  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(OrderSerializer.toBytes(this))

  @ApiModelProperty(hidden = true)
  def getReceiveAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  @ApiModelProperty(hidden = true)
  def getSpendAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

  @ApiModelProperty(hidden = true)
  def getSpendAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
    Try {
      // We should not correct amount here, because it could lead to fork. See ExchangeTransactionDiff
      if (orderType == OrderType.SELL) matchAmount
      else {
        val spend = BigInt(matchAmount) * matchPrice / PriceConstant
        if (getSpendAssetId == Waves && !(spend + matcherFee).isValidLong) {
          throw new ArithmeticException("BigInteger out of long range")
        } else spend.bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  @ApiModelProperty(hidden = true)
  def getReceiveAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
    Try {
      if (orderType == OrderType.BUY) matchAmount
      else {
        (BigInt(matchAmount) * matchPrice / PriceConstant).bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  @ApiModelProperty(hidden = true)
  val json: Coeval[JsObject] = Coeval.evalOnce(OrderSerializer.toJson(this))

  @ApiModelProperty(hidden = true)
  def jsonStr: String = Json.stringify(json())

  @ApiModelProperty(hidden = true)
  override def toString: String = {
    val matcherFeeAssetIdStr = if (version == 3) s" matcherFeeAssetId=${matcherFeeAssetId.fold("Waves")(_.toString)}," else ""
    s"OrderV$version(id=${idStr()}, sender=$senderPublicKey, matcher=$matcherPublicKey, pair=$assetPair, tpe=$orderType, amount=$amount, price=$price, ts=$timestamp, exp=$expiration, fee=$matcherFee,$matcherFeeAssetIdStr proofs=$proofs)"
  }
}

object Order {
  type Id      = ByteStr
  type Version = Byte

  implicit val jsonFormat = com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat

  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant     = 100000000L
  val MaxAmount: Long   = 100 * PriceConstant * PriceConstant

  val V1: Version = 1
  val V2: Version = 2
  val V3: Version = 3

  implicit def sign(order: Order, privateKey: PrivateKey): Order =
    order.copy(proofs = Proofs(crypto.sign(privateKey, order.bodyBytes())))

  def selfSigned(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      assetPair: AssetPair,
      orderType: OrderType,
      amount: TxTimestamp,
      price: TxTimestamp,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: TxTimestamp,
      matcherFeeAssetId: Asset = Asset.Waves
  ): Order =
    Order(version, sender, matcher, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId).signWith(sender)

  def buy(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: TxTimestamp,
      price: TxTimestamp,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: TxTimestamp,
      matcherFeeAssetId: Asset = Waves
  ): Order = {
    Order.selfSigned(version, sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
  }

  def sell(
      version: TxVersion,
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: TxTimestamp,
      price: TxTimestamp,
      timestamp: TxTimestamp,
      expiration: TxTimestamp,
      matcherFee: TxTimestamp,
      matcherFeeAssetId: Asset = Waves
  ): Order = {
    Order.selfSigned(version, sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
  }

  def parseBytes(version: Version, bytes: Array[Byte]): Try[Order] =
    OrderSerializer.parseBytes(version, bytes)
}
