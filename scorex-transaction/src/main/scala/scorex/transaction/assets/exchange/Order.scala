package scorex.transaction.assets.exchange

import com.google.common.primitives.Longs
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.{BytesSerializable, Deser, JsonSerializable}
import scorex.transaction.AssetId
import scorex.utils.ByteArray

import scala.util.Try

/**
  * Order to matcher service for asset exchange
  */
case class Order(sender: PublicKeyAccount, matcher: PublicKeyAccount, spendAssetId: AssetId,
                 receiveAssetId: AssetId, price: Long, amount: Long, maxTimestamp: Long, matcherFee: Long,
                 signature: Array[Byte]) extends BytesSerializable with JsonSerializable {

  import Order._

  /**
    * In what assets is price
    */
  lazy val priceAssetId: AssetId = if (ByteArray.compare(spendAssetId, receiveAssetId) > 0) receiveAssetId
  else spendAssetId

  def isValid(atTime: Long): Boolean = {
    amount > 0 && price > 0 && maxTimestamp - atTime <= MaxLiveTime && atTime <= maxTimestamp &&
      EllipticCurveImpl.verify(signature, toSign, sender.publicKey)
  }

  lazy val toSign: Array[Byte] = sender.publicKey ++ matcher.publicKey ++ spendAssetId ++ receiveAssetId ++
    Longs.toByteArray(price) ++ Longs.toByteArray(amount) ++ Longs.toByteArray(maxTimestamp) ++
    Longs.toByteArray(matcherFee)

  override def bytes: Array[Byte] = toSign ++ signature

  override def json: JsObject = Json.obj(
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

}

object Order extends Deser[Order] {
  val MaxLiveTime: Long = 30L * 24L * 60L * 60L * 1000L
  private val AssetIdLength = 32

  def apply(sender: PrivateKeyAccount, matcher: PublicKeyAccount, spendAssetID: Array[Byte],
            receiveAssetID: Array[Byte], price: Long, amount: Long, maxTime: Long, matcherFee: Long): Order = {
    val unsigned = Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee, Array())
    val sig = EllipticCurveImpl.sign(sender, unsigned.toSign)
    Order(sender, matcher, spendAssetID, receiveAssetID, price, amount, maxTime, matcherFee, sig)
  }

  override def parseBytes(bytes: Array[Byte]): Try[Order] = Try {
    val sender = new PublicKeyAccount(bytes.slice(0, Account.AddressLength))
    val matcher = new PublicKeyAccount(bytes.slice(Account.AddressLength, 2 * Account.AddressLength))
    val spend = bytes.slice(2 * Account.AddressLength, 2 * Account.AddressLength + AssetIdLength)
    val receive = bytes.slice(2 * Account.AddressLength + AssetIdLength, 2 * Account.AddressLength + 2 * AssetIdLength)
    val longsStart = 2 * Account.AddressLength + 2 * AssetIdLength
    val price = Longs.fromByteArray(bytes.slice(longsStart, longsStart + 8))
    val amount = Longs.fromByteArray(bytes.slice(longsStart + 8, longsStart + 16))
    val maxTimestamp = Longs.fromByteArray(bytes.slice(longsStart + 16, longsStart + 24))
    val matcherFee = Longs.fromByteArray(bytes.slice(longsStart + 24, longsStart + 32))
    val signature = bytes.slice(longsStart + 32, bytes.length)
    Order(sender, matcher, spend, receive, price, amount, maxTimestamp, matcherFee, signature)
  }
}
