package scorex.transaction.assets.exchange

import com.google.common.primitives.{Bytes, Longs}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser, JsonSerializable}
import scorex.transaction._
import scorex.utils.{ByteArray, NTP}

import scala.util.Try
import io.swagger.annotations.ApiModelProperty
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.assets.exchange.OrderType.OrderType
import Validation.booleanOperators
import scorex.transaction.assets.TransferTransaction

object OrderType extends Enumeration {
  type OrderType = Value
  val BUY, SELL = Value
}

/**
  * Order to matcher service for asset exchange
  */
@SerialVersionUID(2455530529543215878L)
case class Order(@ApiModelProperty(dataType = "java.lang.String") sender: PublicKeyAccount,
                 @ApiModelProperty(dataType = "java.lang.String", example = "") matcher: PublicKeyAccount,
                 @ApiModelProperty(dataType = "java.lang.String") spendAssetId: Option[AssetId],
                 @ApiModelProperty(dataType = "java.lang.String") receiveAssetId: Option[AssetId],
                 @ApiModelProperty(value = "Price for AssetPair.second in AssetPair.first * 10^8",
                   example = "100000000") price: Long,
                 @ApiModelProperty("Amount in AssetPair.first") amount: Long,
                 @ApiModelProperty(value = "Order time to live, max = 30 days")maxTimestamp: Long,
                 @ApiModelProperty(example = "100000") matcherFee: Long,
                 @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte])
  extends BytesSerializable
  with JsonSerializable {

  import Order._

  def assetPair: AssetPair = AssetPair(spendAssetId, receiveAssetId)

  def orderType: OrderType = if (ByteArray.sameOption(receiveAssetId, assetPair.second)) OrderType.BUY else OrderType.SELL

  @ApiModelProperty(hidden = true)
  lazy val signatureValid = EllipticCurveImpl.verify(signature, toSign, sender.publicKey)

  def isValid(atTime: Long): Validation = {
    (amount > 0) :| "amount should be > 0" &&
      (price > 0) :| "price should be > 0" &&
      (amount < MaxAmount) :| "amount too large" &&
      (getSpendAmount() > 0) :| "spendAmount should be > 0" &&
      (getReceiveAmount() > 0) :| "receiveAmount should be > 0" &&
      (matcherFee > 0) :| "matcherFee should be > 0" &&
      (matcherFee < MaxAmount) :| "matcherFee too large" &&
      (maxTimestamp - atTime <= MaxLiveTime) :| "maxTimestamp should be earlier than 30 days" &&
      (atTime <= maxTimestamp) :| "maxTimestamp should be > currentTime" &&
      !ByteArray.sameOption(spendAssetId, receiveAssetId) :| "Invalid AssetPair" &&
      signatureValid :| "signature should be valid"
  }

  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = sender.publicKey ++ matcher.publicKey ++
    assetIdBytes(spendAssetId) ++ assetIdBytes(receiveAssetId) ++
    Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++ Longs.toByteArray(maxTimestamp) ++
    Longs.toByteArray(matcherFee)

  @ApiModelProperty(hidden = true)
  lazy val id: Array[Byte] = FastCryptographicHash(toSign)

  @ApiModelProperty(hidden = true)
  lazy val idStr: String = Base58.encode(id)

  override def bytes: Array[Byte] = toSign ++ signature

  @ApiModelProperty(hidden = true)
  def getSpendAmount(matchPrice: Long = price, matchAmount: Long = amount): Long = {
    if (orderType == OrderType.BUY) matchAmount
    else (BigInt(matchAmount) * PriceConstant / matchPrice).longValue()
  }

  @ApiModelProperty(hidden = true)
  def getReceiveAmount(matchPrice: Long = price, matchAmount: Long = amount): Long = {
    if (orderType == OrderType.SELL) matchAmount
    else (BigInt(matchAmount) * PriceConstant / matchPrice).longValue()
  }

  override def json: JsObject = Json.obj(
    "id" -> Base58.encode(id),
    "sender" -> Base58.encode(sender.publicKey),
    "matcher" -> Base58.encode(matcher.publicKey),
    "spendAssetId" -> spendAssetId.map(Base58.encode),
    "receiveAssetId" -> receiveAssetId.map(Base58.encode),
    "price" -> price,
    "amount" -> amount,
    "maxTimestamp" -> maxTimestamp,
    "matcherFee" -> matcherFee,
    "signature" -> Base58.encode(signature)
  )

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Order]

  override def equals(obj: Any): Boolean = {
    obj match {
      case o:Order => o.canEqual(this) &&
        sender == o.sender &&
        matcher == o.matcher &&
        ByteArray.sameOption(spendAssetId, o.spendAssetId) &&
        ByteArray.sameOption(receiveAssetId, o.receiveAssetId) &&
        price == o.price &&
        amount == o.amount &&
        maxTimestamp == o.maxTimestamp &&
        matcherFee == o.matcherFee &&
        (signature sameElements o.signature)
      case _ => false
    }
  }

  override def hashCode(): Int = super.hashCode()
}

object Order extends Deser[Order] {
  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  val PriceConstant = 100000000L
  val MaxAmount = PriceConstant*PriceConstant
  private val AssetIdLength = 32


  def buy(sender: PrivateKeyAccount, matcher: PublicKeyAccount, pair: AssetPair,
          price: Long, amount: Long, maxTime: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair.first, pair.second, price, amount, maxTime, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def sell(sender: PrivateKeyAccount, matcher: PublicKeyAccount, pair: AssetPair,
          price: Long, amount: Long, maxTime: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair.second, pair.first, price, amount, maxTime, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def apply(sender: PrivateKeyAccount, matcher: PublicKeyAccount, spendAssetID: Option[AssetId],
            receiveAssetID: Option[AssetId], price: Long, amount: Long, maxTime: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee, sig)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Order] = Try {
    import EllipticCurveImpl._
    var from = 0
    val sender = new PublicKeyAccount(bytes.slice(from, from + KeyLength)); from += KeyLength
    val matcher = new PublicKeyAccount(bytes.slice(from, from + KeyLength)); from += KeyLength
    val (spendAssetId, s0) = parseOption(bytes, from, AssetIdLength); from = s0
    val (receiveAssetId, s1) = parseOption(bytes, from, AssetIdLength); from = s1
    val price = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val amount = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val maxTimestamp = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val matcherFee = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val signature = bytes.slice(from, from + SignatureLength); from += SignatureLength
    Order(sender, matcher, spendAssetId, receiveAssetId, price, amount, maxTimestamp, matcherFee, signature)
  }

  def sign(unsigned: Order, sender: PrivateKeyAccount): Order = {
    require(unsigned.sender == sender)
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
