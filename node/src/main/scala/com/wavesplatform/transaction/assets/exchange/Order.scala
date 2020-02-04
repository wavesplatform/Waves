package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.{BytesSerializable, JsonSerializable}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.Validation.booleanOperators
import com.wavesplatform.utils.byteStrWrites
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
  * Order to matcher service for asset exchange
  */
trait Order extends BytesSerializable with JsonSerializable with Proven {
  def senderPublicKey: PublicKey
  def matcherPublicKey: PublicKey
  def assetPair: AssetPair
  def orderType: OrderType
  def amount: Long
  def price: Long
  def timestamp: Long
  def expiration: Long
  def matcherFee: Long
  def proofs: Proofs
  def version: Byte
  def signature: Array[Byte]   = proofs.toSignature
  def matcherFeeAssetId: Asset = Waves

  import Order._

  val sender = senderPublicKey

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

  val bodyBytes: Coeval[Array[Byte]]
  val id: Coeval[ByteStr]   = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
  val idStr: Coeval[String] = Coeval.evalOnce(id().base58)
  val bytes: Coeval[Array[Byte]]

  def getReceiveAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.amountAsset
    case OrderType.SELL => assetPair.priceAsset
  }

  def getSpendAssetId: Asset = orderType match {
    case OrderType.BUY  => assetPair.priceAsset
    case OrderType.SELL => assetPair.amountAsset
  }

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

  def getReceiveAmount(matchAmount: Long, matchPrice: Long): Either[ValidationError, Long] =
    Try {
      if (orderType == OrderType.BUY) matchAmount
      else {
        (BigInt(matchAmount) * matchPrice / PriceConstant).bigInteger.longValueExact()
      }
    }.toEither.left.map(x => GenericError(x.getMessage))

  override val json: Coeval[JsObject] = Coeval.evalOnce({
    val sig = Base58.encode(signature)
    Json.obj(
      "version"          -> version,
      "id"               -> idStr(),
      "sender"           -> senderPublicKey.stringRepr,
      "senderPublicKey"  -> Base58.encode(senderPublicKey),
      "matcherPublicKey" -> Base58.encode(matcherPublicKey),
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

  override def toString: String = {
    val matcherFeeAssetIdStr = if (version == 3) s" matcherFeeAssetId=${matcherFeeAssetId.fold("Waves")(_.toString)}," else ""
    s"OrderV$version(id=${idStr()}, sender=$senderPublicKey, matcher=$matcherPublicKey, pair=$assetPair, tpe=$orderType, amount=$amount, price=$price, ts=$timestamp, exp=$expiration, fee=$matcherFee,$matcherFeeAssetIdStr proofs=$proofs)"
  }
}

object Order {
  type Id = ByteStr

  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant     = 100000000L
  val MaxAmount: Long   = 100 * PriceConstant * PriceConstant

  def apply(
      senderPublicKey: PublicKey,
      matcherPublicKey: PublicKey,
      assetPair: AssetPair,
      orderType: OrderType,
      amount: Long,
      price: Long,
      timestamp: Long,
      expiration: Long,
      matcherFee: Long,
      proofs: Proofs,
      version: Byte = 1,
      matcherFeeAssetId: Asset = Asset.Waves
  ): Order = version match {
    case 1 => OrderV1(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, proofs)
    case 2 => OrderV2(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, proofs)
    case 3 =>
      OrderV3(senderPublicKey, matcherPublicKey, assetPair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, proofs)
    case _ => throw new IllegalArgumentException(s"Invalid order version: $version")
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def correctAmount(o: Order): Long = correctAmount(o.amount, o.price)

  def buy(
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: Long,
      price: Long,
      timestamp: Long,
      expiration: Long,
      matcherFee: Long,
      version: Byte = 1,
      matcherFeeAssetId: Asset = Waves
  ): Order = {
    val unsigned = version match {
      case 3 =>
        Order(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version, matcherFeeAssetId)
      case _ => Order(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    }
    sign(unsigned, sender)
  }

  def sell(
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      amount: Long,
      price: Long,
      timestamp: Long,
      expiration: Long,
      matcherFee: Long,
      version: Byte = 1,
      matcherFeeAssetId: Asset = Waves
  ): Order = {
    val unsigned = version match {
      case 3 =>
        Order(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version, matcherFeeAssetId)
      case _ => Order(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    }
    sign(unsigned, sender)
  }

  def apply(
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      orderType: OrderType,
      amount: Long,
      price: Long,
      timestamp: Long,
      expiration: Long,
      matcherFee: Long,
      version: Byte
  ): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version)
    sign(unsigned, sender)
  }

  def apply(
      sender: KeyPair,
      matcher: PublicKey,
      pair: AssetPair,
      orderType: OrderType,
      amount: Long,
      price: Long,
      timestamp: Long,
      expiration: Long,
      matcherFee: Long,
      version: Byte,
      matcherFeeAssetId: Asset
  ): Order = {
    val unsigned = Order(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, Proofs.empty, version, matcherFeeAssetId)
    sign(unsigned, sender)
  }

  def sign(unsigned: Order, sender: KeyPair): Order = {
    require(unsigned.senderPublicKey == sender.publicKey)
    val sig = crypto.sign(sender, unsigned.bodyBytes())
    unsigned.updateProofs(Proofs(Seq(ByteStr(sig))))
  }

  def splitByType(o1: Order, o2: Order): (Order, Order) = {
    require(o1.orderType != o2.orderType)
    if (o1.orderType == OrderType.BUY) (o1, o2)
    else (o2, o1)
  }

  def assetIdBytes(assetId: Asset): Array[Byte] = {
    assetId.byteRepr
  }

  def fromBytes(version: Byte, xs: Array[Byte]): Order = version match {
    case 1     => OrderV1.parseBytes(xs).get
    case 2     => OrderV2.parseBytes(xs).get
    case 3     => OrderV3.parseBytes(xs).get
    case other => throw new IllegalArgumentException(s"Unexpected order version: $other")
  }

}
