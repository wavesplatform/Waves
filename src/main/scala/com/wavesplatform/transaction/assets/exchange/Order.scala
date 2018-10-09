package com.wavesplatform.transaction.assets.exchange

import com.google.common.primitives.Longs
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.crypto
import com.wavesplatform.serialization.{BytesSerializable, Deser, JsonSerializable}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.ValidationError.{GenericError, InvalidSignature}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.Validation.booleanOperators
import com.wavesplatform.utils.Base58
import io.swagger.annotations.ApiModelProperty
import monix.eval.{Coeval, Task}
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}

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
case class Order(@ApiModelProperty(dataType = "java.lang.String") senderPublicKey: PublicKeyAccount,
                 @ApiModelProperty(dataType = "java.lang.String", example = "") matcherPublicKey: PublicKeyAccount,
                 assetPair: AssetPair,
                 @ApiModelProperty(dataType = "java.lang.String", example = "buy") orderType: OrderType,
                 @ApiModelProperty("Amount in AssetPair.second") amount: Long,
                 @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8", example = "100000000") price: Long,
                 @ApiModelProperty(value = "Creation timestamp") timestamp: Long,
                 @ApiModelProperty(value = "Order time to live, max = 30 days") expiration: Long,
                 @ApiModelProperty(example = "100000") matcherFee: Long,
                 @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte])
    extends BytesSerializable
    with JsonSerializable
    with Signed {

  import Order._

  val sender: PublicKeyAccount        = senderPublicKey
  val signatureValid: Coeval[Boolean] = Coeval.evalOnce(crypto.verify(signature, toSign, senderPublicKey.publicKey))

  def isValid(atTime: Long): Validation = {
    isValidAmount(amount, price) &&
    assetPair.isValid &&
    (matcherFee > 0) :| "matcherFee should be > 0" &&
    (matcherFee < MaxAmount) :| "matcherFee too large" &&
    (timestamp > 0) :| "timestamp should be > 0" &&
    (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
    (expiration >= atTime) :| "expiration should be > currentTime"
  }

  def isValidAmount(matchAmount: Long, matchPrice: Long): Validation = {
    (matchAmount > 0) :| "amount should be > 0" &&
    (matchPrice > 0) :| "price should be > 0" &&
    (matchAmount < MaxAmount) :| "amount too large" &&
    getSpendAmount(matchAmount, matchPrice).isRight :| "SpendAmount too large" &&
    (getSpendAmount(matchAmount, matchPrice).getOrElse(0L) > 0) :| "SpendAmount should be > 0" &&
    getReceiveAmount(matchAmount, matchPrice).isRight :| "ReceiveAmount too large" &&
    (getReceiveAmount(matchAmount, matchPrice).getOrElse(0L) > 0) :| "ReceiveAmount should be > 0"
  }

  def toSign: Array[Byte] =
    senderPublicKey.publicKey ++ matcherPublicKey.publicKey ++
      assetPair.bytes ++ orderType.bytes ++
      Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
      Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
      Longs.toByteArray(matcherFee)

  @ApiModelProperty(hidden = true)
  val id: Coeval[Id] = Coeval.evalOnce(ByteStr(crypto.fastHash(toSign)))

  @ApiModelProperty(hidden = true)
  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(toSign ++ signature)

  @ApiModelProperty(hidden = true)
  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(toSign)

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
  override val json: Coeval[JsObject] = Coeval.evalOnce(
    Json.obj(
      "id"               -> id().base58,
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
      "signature"        -> Base58.encode(signature)
    ))

  def jsonStr: String = Json.stringify(json())

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Order]

  override def equals(obj: Any): Boolean = {
    obj match {
      case o: Order =>
        o.canEqual(this) &&
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

  override def hashCode(): Int = id().hashCode()

  @ApiModelProperty(hidden = true)
  def getSignedDescendants: Coeval[Seq[Signed]] = signedDescendants
  @ApiModelProperty(hidden = true)
  def getSignaturesValidMemoized: Task[Either[InvalidSignature, Order.this.type]] = signaturesValidMemoized
  @ApiModelProperty(hidden = true)
  def getSignaturesValid: Coeval[Either[InvalidSignature, Order.this.type]] = signaturesValid
}

object Order {
  type Id = ByteStr

  val MaxLiveTime: Long     = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant         = 100000000L
  val MaxAmount: Long       = 100 * PriceConstant * PriceConstant
  private val AssetIdLength = 32

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
          matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Array())
    val sig      = crypto.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def sell(sender: PrivateKeyAccount,
           matcher: PublicKeyAccount,
           pair: AssetPair,
           amount: Long,
           price: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Array())
    val sig      = crypto.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def apply(sender: PrivateKeyAccount,
            matcher: PublicKeyAccount,
            pair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Array())
    val sig      = crypto.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
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
    Order(
      sender,
      matcher,
      AssetPair(amountAssetId.map(ByteStr(_)), priceAssetId.map(ByteStr(_))),
      OrderType(orderType),
      amount,
      price,
      timestamp,
      expiration,
      matcherFee,
      signature
    )
  }

  def sign(unsigned: Order, sender: PrivateKeyAccount): Order = {
    require(unsigned.senderPublicKey == sender)
    val sig = crypto.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
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
