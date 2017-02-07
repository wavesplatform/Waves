package scorex.transaction.assets.exchange

import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.utils.ByteArrayExtension

import scala.util.Try

case class AssetPair(private val pair: (Option[AssetId], Option[AssetId])) {
  require(!ByteArrayExtension.sameOption(pair._1, pair._2))
  val first: Option[AssetId] = if (ByteArrayExtension.compare(pair._1, pair._2) < 0) pair._1 else pair._2
  val second: Option[AssetId] = if (ByteArrayExtension.compare(pair._1, pair._2) < 0) pair._2 else pair._1

  lazy val firstStr: String = first.map(Base58.encode).getOrElse(AssetPair.WavesName)
  lazy val secondStr: String = second.map(Base58.encode).getOrElse(AssetPair.WavesName)

  override def hashCode(): Int = toString.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: AssetPair =>
        toString == other.toString
      case _ => false
    }
  }

  override def toString: String = firstStr + "-" + secondStr
}

object AssetPair {
  val WavesName = "WAVES"

  def createAssetPair(asset1: String, asset2: String): Try[AssetPair] = {
    def tryAssetId(a: String) = Try { if (WavesName == a) None else Some(Base58.decode(a).get) }
    (for {
      a1 <- tryAssetId(asset1)
      a2 <- tryAssetId(asset2)
    } yield Try(AssetPair(a1, a2))).flatten
  }
}