package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.crypto
import com.wavesplatform.serialization.{BytesSerializable, JsonSerializable}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.Validation.booleanOperators
import com.wavesplatform.utils.Base58
import io.swagger.annotations.ApiModelProperty
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

sealed trait OrderType {
  def bytes: Array[Byte]
}

object OrderType {

  case object BUY extends OrderType {
    def bytes: Array[Byte] = Array(0.toByte)

    override def toString: String = "buy"
  }

  case object SELL extends OrderType {
    def bytes: Array[Byte] = Array(1.toByte)

    override def toString: String = "sell"
  }

  def apply(value: Int): OrderType = value match {
    case 0 => OrderType.BUY
    case 1 => OrderType.SELL
    case _ => throw new RuntimeException(s"Unexpected OrderType: $value")
  }

  def apply(value: String): OrderType = value match {
    case "buy"  => OrderType.BUY
    case "sell" => OrderType.SELL
    case _      => throw new RuntimeException("Unexpected OrderType")
  }

  def reverse(orderType: OrderType): OrderType = orderType match {
    case BUY  => SELL
    case SELL => BUY
  }
}

/**
  * Order to matcher service for asset exchange
  */
trait Order extends BytesSerializable with JsonSerializable with Proven {
  @ApiModelProperty(dataType = "java.lang.String") def senderPublicKey: PublicKeyAccount
  @ApiModelProperty(dataType = "java.lang.String", example = "") def matcherPublicKey: PublicKeyAccount
  def assetPair: AssetPair
  @ApiModelProperty(dataType = "java.lang.String", example = "buy") def orderType: OrderType
  @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8", example = "100000000") def price: Long
  @ApiModelProperty("Amount in AssetPair.second") def amount: Long
  @ApiModelProperty(value = "Creation timestamp") def timestamp: Long
  @ApiModelProperty(value = "Order time to live, max = 30 days") def expiration: Long
  @ApiModelProperty(example = "100000") def matcherFee: Long
  @ApiModelProperty(dataType = "Proofs") def proofs: Proofs

  def version: Byte

  def signature: Array[Byte] = proofs.proofs(0).arr

  import Order._

  val sender = senderPublicKey

  def isValid(atTime: Long): Validation = {
    isValidAmount(price, amount) &&
    assetPair.isValid &&
    (matcherFee > 0) :| "matcherFee should be > 0" &&
    (matcherFee < MaxAmount) :| "matcherFee too large" &&
    (timestamp > 0) :| "timestamp should be > 0" &&
    (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
    (expiration >= atTime) :| "expiration should be > currentTime"
  }

  def isValidAmount(matchPrice: Long, matchAmount: Long): Validation = {
    (matchAmount > 0) :| "amount should be > 0" &&
    (matchPrice > 0) :| "price should be > 0" &&
    (matchAmount < MaxAmount) :| "amount too large" &&
    getSpendAmount(matchPrice, matchAmount).isRight :| "SpendAmount too large" &&
    (getSpendAmount(matchPrice, matchAmount).getOrElse(0L) > 0) :| "SpendAmount should be > 0" &&
    getReceiveAmount(matchPrice, matchAmount).isRight :| "ReceiveAmount too large" &&
    (getReceiveAmount(matchPrice, matchAmount).getOrElse(0L) > 0) :| "ReceiveAmount should be > 0"
  }

  @ApiModelProperty(hidden = true)
  val bodyBytes: Coeval[Array[Byte]]

  @ApiModelProperty(hidden = true)
  val id: Coeval[Array[Byte]] = Coeval.evalOnce(crypto.fastHash(bodyBytes()))

  @ApiModelProperty(hidden = true)
  val idStr: Coeval[String] = Coeval.evalOnce(Base58.encode(id()))

  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]]

  @ApiModelProperty(hidden = true)
  def getReceiveAssetId: Option[AssetId] = orderType match {
    case OrderType.BUY  => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  @ApiModelProperty(hidden = true)
  def getSpendAssetId: Option[AssetId] = orderType match {
    case OrderType.BUY  => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

  @ApiModelProperty(hidden = true)
  def getSpendAmount(matchPrice: Long, matchAmount: Long): Either[ValidationError, Long] =
    Try {
      // We should not correct amount here, because it could lead to fork. See ExchangeTransactionDiff
      if (orderType == OrderType.SELL) matchAmount
      else {
        val spend = BigInt(matchAmount) * matchPrice / PriceConstant
        if (getSpendAssetId.isEmpty && !(spend + matcherFee).isValidLong) {
          throw new ArithmeticException("BigInteger out of long range")
        } else spend.bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  @ApiModelProperty(hidden = true)
  def getReceiveAmount(matchPrice: Long, matchAmount: Long): Either[ValidationError, Long] =
    Try {
      if (orderType == OrderType.BUY) matchAmount
      else {
        (BigInt(matchAmount) * matchPrice / PriceConstant).bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  @ApiModelProperty(hidden = true)
  override val json: Coeval[JsObject] = Coeval.evalOnce({
    val sig = Base58.encode(signature)
    Json.obj(
      "version"          -> version,
      "id"               -> idStr(),
      "sender"           -> senderPublicKey.address,
      "senderPublicKey"  -> Base58.encode(senderPublicKey.publicKey),
      "matcherPublicKey" -> Base58.encode(matcherPublicKey.publicKey),
      "assetPair"        -> assetPair.json,
      "orderType"        -> orderType.toString,
      "price"            -> price,
      "amount"           -> amount,
      "timestamp"        -> timestamp,
      "expiration"       -> expiration,
      "matcherFee"       -> matcherFee,
      "signature"        -> sig,
      "proofs"           -> proofs.proofs
    )
  })

  def jsonStr: String = Json.stringify(json())

  override def equals(obj: Any): Boolean = {
    obj match {
      case o: Order =>
        senderPublicKey == o.senderPublicKey &&
          matcherPublicKey == o.matcherPublicKey &&
          assetPair == o.assetPair &&
          orderType == o.orderType &&
          price == o.price &&
          amount == o.amount &&
          expiration == o.expiration &&
          matcherFee == o.matcherFee &&
          (signature sameElements o.signature)
      case _ => false
    }
  }

  override def hashCode(): Int = idStr.hashCode()
}

object Order {
  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant     = 100000000L
  val MaxAmount: Long   = 100 * PriceConstant * PriceConstant

  def apply(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
            @ApiModelProperty(dataType = "java.lang.String", example = "") matcherPublicKey: PublicKeyAccount,
            assetPair: AssetPair,
            @ApiModelProperty(dataType = "java.lang.String", example = "buy") orderType: OrderType,
            @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8", example = "100000000") price: Long,
            @ApiModelProperty("Amount in AssetPair.second") amount: Long,
            @ApiModelProperty(value = "Creation timestamp") timestamp: Long,
            @ApiModelProperty(value = "Order time to live, max = 30 days") expiration: Long,
            @ApiModelProperty(example = "100000") matcherFee: Long,
            @ApiModelProperty(dataType = "Proofs") proofs: Proofs,
            @ApiModelProperty(dataType = "java.lang.Byte") version: Byte = 1): Order = {
    if (version == 1) {
      OrderV1(senderPublicKey, matcherPublicKey, assetPair, orderType, price, amount, timestamp, expiration, matcherFee, proofs)
    } else {
      OrderV2(senderPublicKey, matcherPublicKey, assetPair, orderType, price, amount, timestamp, expiration, matcherFee, proofs)
    }
  }
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

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def correctAmount(o: Order): Long = correctAmount(o.amount, o.price)

  def buy(sender: PrivateKeyAccount,
          matcher: PublicKeyAccount,
          pair: AssetPair,
          price: Long,
          amount: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long,
          version: Byte = 1): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.BUY, price, amount, timestamp, expiration, matcherFee, Proofs.empty, version)
    sign(unsigned, sender)
  }

  def sell(sender: PrivateKeyAccount,
           matcher: PublicKeyAccount,
           pair: AssetPair,
           price: Long,
           amount: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long,
           version: Byte = 1): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.SELL, price, amount, timestamp, expiration, matcherFee, Proofs.empty, version)
    sign(unsigned, sender)
  }

  def apply(sender: PrivateKeyAccount,
            matcher: PublicKeyAccount,
            pair: AssetPair,
            orderType: OrderType,
            price: Long,
            amount: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            version: Byte): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee, Proofs.empty, version)
    sign(unsigned, sender)
  }

  def sign(unsigned: Order, sender: PrivateKeyAccount): Order = {
    require(unsigned.senderPublicKey == sender)
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned match {
      case o @ OrderV2(_, _, _, _, _, _, _, _, _, _) =>
        o.copy(proofs = Proofs(Seq(ByteStr(sig))))
      case o @ OrderV1(_, _, _, _, _, _, _, _, _, _) =>
        o.copy(proofs = Proofs(Seq(ByteStr(sig))))
    }
  }

  def splitByType(o1: Order, o2: Order): (Order, Order) = {
    require(o1.orderType != o2.orderType)
    if (o1.orderType == OrderType.BUY) (o1, o2)
    else (o2, o1)
  }

  def assetIdBytes(assetId: Option[AssetId]): Array[Byte] = {
    assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
  }

}
