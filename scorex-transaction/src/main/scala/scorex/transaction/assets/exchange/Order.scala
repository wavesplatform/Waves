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
import Validation.BooleanOperators
import scorex.transaction.assets.TransferTransaction

object OrderType extends Enumeration {
  type OrderType = Value
  val BUY, SELL = Value
}

/**
  * Order to matcher service for asset exchange
  */
case class Order(@ApiModelProperty(dataType = "java.lang.String") sender: PublicKeyAccount,
                 @ApiModelProperty(dataType = "java.lang.String", example = "") matcher: PublicKeyAccount,
                 @ApiModelProperty(dataType = "java.lang.String") spendAssetId: AssetId,
                 @ApiModelProperty(dataType = "java.lang.String") receiveAssetId: AssetId,
                 @ApiModelProperty("Price for AssetPair.second in AssetPair.first * 10^8") price: Long,
                 @ApiModelProperty("Amount in AssetPair.first") amount: Long,
                 maxTimestamp: Long,
                 @ApiModelProperty(example = "1000") matcherFee: Long,
                 @ApiModelProperty(dataType = "java.lang.String") signature: Array[Byte])
  extends BytesSerializable
  with JsonSerializable {

  import Order._

  def assetPair: AssetPair = AssetPair(spendAssetId, receiveAssetId)

  def orderType: OrderType = if (receiveAssetId sameElements assetPair.first) OrderType.BUY else OrderType.SELL

  def isValid(atTime: Long): Validation = {
    (amount > 0) :| "amount should be > 0" &&
      (price > 0) :| "price should be > 0" &&
      (maxTimestamp - atTime <= MaxLiveTime) :| "maxTimestamp should be earlier than 30 days" &&
      (atTime <= maxTimestamp) :| "maxTimestamp should be > currentTime" &&
      EllipticCurveImpl.verify(signature, toSign, sender.publicKey) :| "signature should be valid"
  }

  @ApiModelProperty(hidden = true)
  lazy val toSign: Array[Byte] = sender.publicKey ++ matcher.publicKey ++ spendAssetId ++ receiveAssetId ++
    Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++ Longs.toByteArray(maxTimestamp) ++
    Longs.toByteArray(matcherFee)

  @ApiModelProperty(hidden = true)
  lazy val id: Array[Byte] = FastCryptographicHash(toSign)

  override def bytes: Array[Byte] = toSign ++ signature

  override def json: JsObject = Json.obj(
    "id" -> Base58.encode(id),
    "sender" -> sender.address,
    "matcher" -> matcher.address,
    "spendAssetId" -> Base58.encode(spendAssetId),
    "receiveAssetId" -> Base58.encode(receiveAssetId),
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
        (spendAssetId sameElements o.spendAssetId) &&
        (receiveAssetId sameElements o.receiveAssetId) &&
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
  private val AssetIdLength = 32


  def buy(sender: PrivateKeyAccount, matcher: PublicKeyAccount, pair: AssetPair,
          price: Long, amount: Long, maxTime: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair.second, pair.first, price, amount, maxTime, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def sell(sender: PrivateKeyAccount, matcher: PublicKeyAccount, pair: AssetPair,
          price: Long, amount: Long, maxTime: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, pair.first, pair.second, price, amount, maxTime, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    unsigned.copy(signature = sig)
  }

  def apply(sender: PrivateKeyAccount, matcher: PublicKeyAccount, spendAssetID: Array[Byte],
            receiveAssetID: Array[Byte], price: Long, amount: Long, maxTime: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee, sig)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Order] = Try {
    import EllipticCurveImpl._
    var from = 0
    val sender = new PublicKeyAccount(bytes.slice(from, from + KeyLength)); from += KeyLength
    val matcher = new PublicKeyAccount(bytes.slice(from, from + KeyLength)); from += KeyLength
    val spendAssetId = bytes.slice(from, from + AssetIdLength); from += AssetIdLength
    val receiveAssetId = bytes.slice(from, from + AssetIdLength); from += AssetIdLength
    val price = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val amount = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val maxTimestamp = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val matcherFee = Longs.fromByteArray(bytes.slice(from, from + AssetIdLength)); from += 8
    val signature = bytes.slice(from, from + SignatureLength); from += SignatureLength
    Order(sender, matcher, spendAssetId, receiveAssetId, price, amount, maxTimestamp, matcherFee, signature)
  }
}
