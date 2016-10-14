package scorex.transaction.assets.exchange

import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.utils.ByteArray

case class AssetPair(private val pair: (AssetId, AssetId)) {
  val first = if (ByteArray.compare(pair._1, pair._2) < 0) pair._1 else pair._2
  val second = if (ByteArray.compare(pair._1, pair._2) < 0) pair._2 else pair._1

  lazy val key: String = Base58.encode(first) + Base58.encode(second)

  override def hashCode(): Int = key.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: AssetPair =>
        key == other.key
      case _ => false
    }
  }
}
