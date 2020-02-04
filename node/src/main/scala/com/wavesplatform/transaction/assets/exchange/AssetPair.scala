package com.wavesplatform.transaction.assets.exchange

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.Order.assetIdBytes
import com.wavesplatform.transaction.assets.exchange.Validation.booleanOperators
import net.ceedubs.ficus.readers.ValueReader
import play.api.libs.json.{JsObject, Json}

import scala.util.{Success, Try}

case class AssetPair(amountAsset: Asset, priceAsset: Asset) {
  import AssetPair._

  lazy val priceAssetStr: String  = assetIdStr(priceAsset)
  lazy val amountAssetStr: String = assetIdStr(amountAsset)
  override def toString: String   = key
  def key: String                 = amountAssetStr + "-" + priceAssetStr
  def isValid: Validation         = (amountAsset != priceAsset) :| "Invalid AssetPair"
  def bytes: Array[Byte]          = assetIdBytes(amountAsset) ++ assetIdBytes(priceAsset)
  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.maybeBase58Repr,
    "priceAsset"  -> priceAsset.maybeBase58Repr
  )
  def reverse = AssetPair(priceAsset, amountAsset)

  def assets: Set[Asset] = Set(amountAsset, priceAsset)
}

object AssetPair {
  val WavesName = "WAVES"

  def assetIdStr(aid: Asset): String = aid match {
    case Waves           => WavesName
    case IssuedAsset(id) => id.base58
  }

  def extractAssetId(a: String): Try[Asset] = a match {
    case `WavesName` => Success(Waves)
    case other       => ByteStr.decodeBase58(other).map(IssuedAsset)
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)

  def fromBytes(xs: Array[Byte]): AssetPair = {
    val (amount, offset) = Deser.parseByteArrayOption(xs, 0, AssetIdLength)
    val (price, _)       = Deser.parseByteArrayOption(xs, offset, AssetIdLength)
    AssetPair(
      Asset.fromCompatId(amount.map(ByteStr(_))),
      Asset.fromCompatId(price.map(ByteStr(_)))
    )
  }

  implicit val assetPairReader: ValueReader[AssetPair] = { (cfg, path) =>
    val source    = cfg.getString(path)
    val sourceArr = source.split("-")
    val res = sourceArr match {
      case Array(amtAssetStr, prcAssetStr) => AssetPair.createAssetPair(amtAssetStr, prcAssetStr)
      case _                               => throw new Exception(s"$source (incorrect assets count, expected 2 but got ${sourceArr.size})")
    }
    res fold (ex => throw new Exception(s"$source (${ex.getMessage})"), identity)
  }
}
