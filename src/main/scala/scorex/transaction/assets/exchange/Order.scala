package scorex.transaction.assets.exchange

import com.google.common.primitives.Longs
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.serialization.{BytesSerializable, Deser, JsonSerializable}
import scorex.transaction.TransactionParser._
import scorex.transaction.ValidationError.CustomError
import scorex.transaction._
import scorex.transaction.assets.exchange.Validation.booleanOperators

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
    case _ => throw new RuntimeException("Unexpected OrderType")
  }

  def apply(value: String): OrderType = value match {
    case "buy" => OrderType.BUY
    case "sell" => OrderType.SELL
    case _ => throw new RuntimeException("Unexpected OrderType")
  }

  def reverse(orderType: OrderType): OrderType = orderType match {
    case BUY => SELL
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
                 @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8",
                   example = "100000000") price: Long,
                 @ApiModelProperty("Amount in AssetPair.second") amount: Long,
                 @ApiModelProperty(value = "Creation timestamp") timestamp: Long,
                 @ApiModelProperty(value = "Order time to live, max = 30 days") expiration: Long,
                 @ApiModelProperty(example = "100000") matcherFee: Long,
                 @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte])
  extends BytesSerializable
    with JsonSerializable {

  import Order._

  @ApiModelProperty(hidden = true)
  lazy val signatureValid = EllipticCurveImpl.verify(signature, toSign, senderPublicKey.publicKey)

  def isValid(atTime: Long): Validation = {
    (amount > 0) :| "amount should be > 0" &&
      (price > 0) :| "price should be > 0" &&
      assetPair.isValid &&
      (amount < MaxAmount) :| "amount too large" &&
      getSpendAmount(price, amount).isRight :| "SpendAmount too large" &&
      (getSpendAmount(price, amount).getOrElse(0L) > 0) :| "SpendAmount should be > 0" &&
      getReceiveAmount(price, amount).isRight :| "ReceiveAmount too large" &&
      (getReceiveAmount(price, amount).getOrElse(0L) > 0) :| "ReceiveAmount should be > 0" &&
      (matcherFee > 0) :| "matcherFee should be > 0" &&
      (matcherFee < MaxAmount) :| "matcherFee too large" &&
      (timestamp > 0) :| "timestamp should be > 0" &&
      (expiration - atTime <= MaxLiveTime) :| "expiration should be earlier than 30 days" &&
      (expiration >= atTime) :| "expiration should be > currentTime" &&
      signatureValid :| "signature should be valid"
  }

  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = senderPublicKey.publicKey ++ matcherPublicKey.publicKey ++
    assetPair.bytes ++ orderType.bytes ++
    Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++
    Longs.toByteArray(timestamp) ++ Longs.toByteArray(expiration) ++
    Longs.toByteArray(matcherFee)

  @ApiModelProperty(hidden = true)
  lazy val id: Array[Byte] = FastCryptographicHash(toSign)

  @ApiModelProperty(hidden = true)
  lazy val idStr: String = Base58.encode(id)

  @ApiModelProperty(hidden = true)
  lazy val bytes: Array[Byte] = toSign ++ signature

  @ApiModelProperty(hidden = true)
  def getReceiveAssetId: Option[AssetId] = orderType match {
    case OrderType.BUY => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  @ApiModelProperty(hidden = true)
  def getSpendAssetId: Option[AssetId] = orderType match {
    case OrderType.BUY => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

  @ApiModelProperty(hidden = true)
  def getSpendAmount(matchPrice: Long, matchAmount: Long): Either[ValidationError, Long] = Try {
    if (orderType == OrderType.SELL) matchAmount
    else {
      val spend = BigInt(matchAmount) * matchPrice / PriceConstant
      if (getSpendAssetId.isEmpty && !(spend + matcherFee).isValidLong) {
        throw new ArithmeticException("BigInteger out of long range")
      } else spend.bigInteger.longValueExact()
    }
  }.toEither.left.map(x => CustomError(x.getMessage))

  @ApiModelProperty(hidden = true)
  def getReceiveAmount(matchPrice: Long, matchAmount: Long): Either[ValidationError, Long] = Try {
    if (orderType == OrderType.BUY) matchAmount
    else {
      (BigInt(matchAmount) * matchPrice / PriceConstant).bigInteger.longValueExact()
    }
  }.toEither.left.map(x => CustomError(x.getMessage))

  override def json: JsObject = Json.obj(
    "id" -> Base58.encode(id),
    "senderPublicKey" -> Base58.encode(senderPublicKey.publicKey),
    "matcherPublicKey" -> Base58.encode(matcherPublicKey.publicKey),
    "assetPair" -> assetPair.json,
    "orderType" -> orderType.toString,
    "price" -> price,
    "amount" -> amount,
    "timestamp" -> timestamp,
    "expiration" -> expiration,
    "matcherFee" -> matcherFee,
    "signature" -> Base58.encode(signature)
  )

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Order]

  override def equals(obj: Any): Boolean = {
    obj match {
      case o: Order => o.canEqual(this) &&
        senderPublicKey == o.senderPublicKey &&
        matcherPublicKey == o.matcherPublicKey &&
        assetPair == o.assetPair
        orderType == o.orderType
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
  val PriceConstant = 100000000L
  val MaxAmount: Long = 100 * PriceConstant * PriceConstant
  private val AssetIdLength = 32


  def buy(sender: PrivateKeyAccount, matcher: PublicKeyAccount, pair: AssetPair,
          price: Long, amount: Long, timestamp: Long, expiration: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.BUY, price, amount, timestamp, expiration, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def sell(sender: PrivateKeyAccount, matcher: PublicKeyAccount, pair: AssetPair,
           price: Long, amount: Long, timestamp: Long, expiration: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair, OrderType.SELL, price, amount, timestamp, expiration, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def apply(sender: PrivateKeyAccount, matcher: PublicKeyAccount, pair: AssetPair, orderType: OrderType,
            price: Long, amount: Long, timestamp: Long, expiration: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, price, amount, timestamp, expiration, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def parseBytes(bytes: Array[Byte]): Try[Order] = Try {
    import EllipticCurveImpl._
    var from = 0
    val sender = PublicKeyAccount(bytes.slice(from, from + KeyLength))
    from += KeyLength
    val matcher = PublicKeyAccount(bytes.slice(from, from + KeyLength))
    from += KeyLength
    val (amountAssetId, s0) = Deser.parseOption(bytes, from, AssetIdLength)
    from = s0
    val (priceAssetId, s1) = Deser.parseOption(bytes, from, AssetIdLength)
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
    Order(sender, matcher, AssetPair(amountAssetId, priceAssetId), OrderType(orderType), price, amount, timestamp, expiration, matcherFee, signature)
  }

  def sign(unsigned: Order, sender: PrivateKeyAccount): Order = {
    require(unsigned.senderPublicKey == sender)
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def splitByType(o1: Order, o2: Order): (Order, Order) = {
    require(o1.orderType != o2.orderType)
    if (o1.orderType == OrderType.BUY) (o1, o2)
    else (o2, o1)
  }

  def assetIdBytes(assetId: Option[AssetId]): Array[Byte] = {
    assetId.map(a => (1: Byte) +: a).getOrElse(Array(0: Byte))
  }
}
