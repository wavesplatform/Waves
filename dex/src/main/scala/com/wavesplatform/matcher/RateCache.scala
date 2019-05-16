package com.wavesplatform.matcher

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.iq80.leveldb.DB
import play.api.libs.json.{JsObject, Json}

import scala.collection.JavaConverters._

trait RateCache {

  def upsertRate(asset: Asset, value: Double): Option[Double]

  def getRate(asset: Asset): Option[Double]

  def getAllRates: Map[Asset, Double]

  def deleteRate(asset: Asset): Unit

  def getJson: JsObject = RateCache.getJson(getAllRates)
}

object RateCache {

  def getJson(ratesMap: Map[Asset, Double]): JsObject = Json.obj(
    ratesMap.map { case (asset, rate) => AssetPair.assetIdStr(asset) -> Json.toJsFieldJsValueWrapper(rate) }.toSeq: _*
  )

  def apply(db: DB): RateCache = new RateCache {

    private val rateDB  = RateDB(db)
    private val rateMap = new ConcurrentHashMap[IssuedAsset, Double](rateDB.getAllRates.asJava)

    def upsertRate(asset: Asset, value: Double): Option[Double] =
      asset.fold { Option(1d) } { issuedAsset =>
        rateDB.upsertRate(issuedAsset, value)
        Option(rateMap.put(issuedAsset, value))
      }

    def getRate(asset: Asset): Option[Double] = asset.fold(Option(1d)) { asset =>
      if (rateMap containsKey asset) Some(rateMap get asset) else None
    }

    def getAllRates: Map[Asset, Double] = {
      rateMap.asScala.toMap.map { case (issuedAsset, value) => Asset.fromCompatId(issuedAsset.compatId) -> value } + (Waves -> 1d)
    }

    def deleteRate(asset: Asset): Unit = asset.fold { () } { issuedAsset =>
      rateDB.deleteRate(issuedAsset)
      rateMap.remove(issuedAsset)
    }
  }
}
