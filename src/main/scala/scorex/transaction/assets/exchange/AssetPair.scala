package scorex.transaction.assets.exchange

import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.transaction.assets.exchange.Order.assetIdBytes
import scorex.utils.ByteArrayExtension
import scorex.transaction.assets.exchange.Validation.booleanOperators

import scala.util.{Success, Try}

case class AssetPair(amountAsset: Option[AssetId], priceAsset: Option[AssetId]) {
  lazy val priceAssetStr: String = priceAsset.map(Base58.encode).getOrElse(AssetPair.WavesName)
  lazy val amountAssetStr: String = amountAsset.map(Base58.encode).getOrElse(AssetPair.WavesName)

  override def hashCode(): Int = toString.hashCode()

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: AssetPair =>
        toString == other.toString
      case _ => false
    }
  }

  override def toString: String = amountAssetStr + "-" + priceAssetStr

  def isValid: Validation = {
    !ByteArrayExtension.sameOption(amountAsset, priceAsset) :| "Invalid AssetPair"
  }

  def bytes: Array[Byte] = assetIdBytes(amountAsset) ++ assetIdBytes(priceAsset)

  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.map(Base58.encode),
    "priceAsset" -> priceAsset.map(Base58.encode)
  )
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
