package scorex.transaction.assets.exchange

import com.wavesplatform.state2.EqByteArray
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsObject, Json}
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.transaction.assets.exchange.Order.assetIdBytes
import scorex.utils.ByteArrayExtension
import scorex.transaction.assets.exchange.Validation.booleanOperators

import scala.util.{Success, Try}

case class AssetPair(@ApiModelProperty(dataType = "java.lang.String") amountAsset: Option[AssetId],
                     @ApiModelProperty(dataType = "java.lang.String") priceAsset: Option[AssetId]) {
  @ApiModelProperty(hidden = true)
  lazy val priceAssetStr: String = priceAsset.map(_.base58).getOrElse(AssetPair.WavesName)
  @ApiModelProperty(hidden = true)
  lazy val amountAssetStr: String = amountAsset.map(_.base58).getOrElse(AssetPair.WavesName)

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
    (amountAsset != priceAsset) :| "Invalid AssetPair"
  }

  def bytes: Array[Byte] = assetIdBytes(amountAsset) ++ assetIdBytes(priceAsset)

  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.map(_.base58),
    "priceAsset" -> priceAsset.map(_.base58)
  )
}

object AssetPair {
  val WavesName = "WAVES"

  private def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case `WavesName` => Success(None)
    case other => EqByteArray.decode(other).map(Option(_))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)
}
