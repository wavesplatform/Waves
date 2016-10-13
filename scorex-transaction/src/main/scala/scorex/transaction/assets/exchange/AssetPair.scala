package scorex.transaction.assets.exchange

import scorex.crypto.encode.Base58
import scorex.transaction._

case class AssetPair(asset1: AssetId, asset2: AssetId) {
  lazy val key: String = Base58.encode(asset1) + Base58.encode(asset2)

  override def hashCode(): Int = key.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: AssetPair =>
        key == other.key
      case _ => false
    }
  }
}
