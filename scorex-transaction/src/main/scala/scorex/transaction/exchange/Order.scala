package scorex.transaction.exchange

import com.google.common.primitives.Longs
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.serialization.{BytesSerializable, Deser}
import scorex.utils.{ByteArray, NTP}

import scala.util.Try

case class Order(spendAddress: PublicKeyAccount, matcherAddress: PublicKeyAccount, spendAssetID: Array[Byte],
                 receiveAssetID: Array[Byte], price: Long, amount: Long, maxTimestamp: Long, matcherFee: Long,
                 signature: Array[Byte]) extends BytesSerializable {

  import Order._

  /**
    * In what assets is price
    */
  lazy val priceAssetId: Array[Byte] = if (ByteArray.compare(spendAssetID, receiveAssetID) > 0) receiveAssetID
  else spendAssetID

  /**
    * In what assets is amount
    */
  lazy val amountAsset: Array[Byte] = if (priceAssetId sameElements spendAssetID) receiveAssetID
  else spendAssetID


  def isValid: Boolean = {
    val maxTimestampIsValid: Boolean = {
      val diff = maxTimestamp - NTP.correctedTime()
      diff > MinLivetime && diff < MaxLivetime
    }
    maxTimestampIsValid && EllipticCurveImpl.verify(signature, toSign, spendAddress.publicKey)
  }

  lazy val toSign: Array[Byte] = {
    spendAddress.bytes ++ matcherAddress.bytes ++ spendAssetID ++ receiveAssetID ++ Longs.toByteArray(price) ++
      Longs.toByteArray(amount) ++ Longs.toByteArray(maxTimestamp) ++ Longs.toByteArray(matcherFee)
  }

  override def bytes: Array[Byte] = toSign ++ signature
}

object Order extends Deser[Order] {
  val MinLivetime = 12 * 60 * 60 * 1000
  val MaxLivetime = 30 * 24 * 60 * 60 * 1000

  override def parseBytes(bytes: Array[Byte]): Try[Order] = ???
}