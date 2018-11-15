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

/**
  * Order to matcher service for asset exchange
  */
trait Order extends BytesSerializable with JsonSerializable with Proven {
  def senderPublicKey: PublicKeyAccount

  def matcherPublicKey: PublicKeyAccount

  def assetPair: AssetPair

  def orderType: OrderType

  def amount: Long

  def price: Long

  def timestamp: Long

  def expiration: Long

  def matcherFee: Long

  def proofs: Proofs

  def version: Byte

  def signature: Array[Byte] = proofs.proofs(0).arr

  import Order._

  @ApiModelProperty(hidden = true)
  val sender = senderPublicKey

  @ApiModelProperty(hidden = true)
  def isValid(atTime: Long): Validation = {
    isValidAmount(amount, price) &&
    assetPair.isValid &&
    (matcherFee > 0) :| "matcherFee should be > 0" &&
    (matcherFee < MaxAmount) :| "matcherFee too large" &&
    (timestamp > 0) :| "timestamp should be > 0" &&
    (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
    (expiration >= atTime) :| "expiration should be > currentTime"
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
  val bodyBytes: Coeval[Array[Byte]]
  @ApiModelProperty(hidden = true)
  val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
  @ApiModelProperty(hidden = true)
  val idStr: Coeval[String] = Coeval.evalOnce(id().base58)
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
  def getSpendAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
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
  def getReceiveAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
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
      "amount"           -> amount,
      "price"            -> price,
      "timestamp"        -> timestamp,
      "expiration"       -> expiration,
      "matcherFee"       -> matcherFee,
      "signature"        -> sig,
      "proofs"           -> proofs.proofs
    )
  })

  @ApiModelProperty(hidden = true)
  def jsonStr: String = Json.stringify(json())

  @ApiModelProperty(hidden = true)
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

  @ApiModelProperty(hidden = true)
  override def hashCode(): Int = idStr.hashCode()
}

object Order {
  type Id = ByteStr

  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant     = 100000000L
  val MaxAmount: Long   = 100 * PriceConstant * PriceConstant

  def apply(senderPublicKey: PublicKeyAccount,
            matcherPublicKey: PublicKeyAccount,
            assetPair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            proofs: Proofs,
            version: Byte = 1): Order = {
    if (version == 1) {
      OrderV1(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, proofs)
    } else {
      OrderV2(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, proofs)
    }
  }

  def apply(senderPublicKey: PublicKeyAccount,
            matcherPublicKey: PublicKeyAccount,
            assetPair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            signature: Array[Byte]): Order = {
    OrderV1(senderPublicKey,
            matcherPublicKey,
            assetPair,
            orderType,
            amount,
            price,
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
          amount: Long,
          price: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long,
          version: Byte = 1): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    sign(unsigned, sender)
  }

  def sell(sender: PrivateKeyAccount,
           matcher: PublicKeyAccount,
           pair: AssetPair,
           amount: Long,
           price: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long,
           version: Byte = 1): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    sign(unsigned, sender)
  }

  def apply(sender: PrivateKeyAccount,
            matcher: PublicKeyAccount,
            pair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            version: Byte): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
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

  def fromBytes(xs: Array[Byte]): Order = xs.head match {
    case 1     => OrderV1.parseBytes(xs.tail).get
    case 2     => OrderV2.parseBytes(xs.tail).get
    case other => throw new IllegalArgumentException(s"Unexpected order version: $other")
  }

}
