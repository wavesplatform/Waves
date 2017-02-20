package scorex.transaction.assets.exchange

import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.utils.ByteArrayExtension
import scala.util.{Success, Try}

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

  private def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case `WavesName` => Success(None)
    case other => Base58.decode(other).map(Option(_))
  }

  def createAssetPair(asset1: String, asset2: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(asset1)
      a2 <- extractAssetId(asset2)
    } yield AssetPair(a1, a2)
}
