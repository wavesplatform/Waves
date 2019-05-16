package com.wavesplatform.it.sync

import akka.http.scaladsl.model.StatusCodes._
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

class RatesTestSuite extends MatcherSuiteBase {

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_)).foreach(tx => node.waitForTransaction(tx.id))
  }

  val defaultRateMap: Map[Asset, Double] = Map(Waves -> 1d)
  val wctRate                            = 0.2
  val wctRateUpdated                     = 0.5
  val wctStr                             = WctId.base58
  val wctAsset                           = IssuedAsset(WctId)

  "Rates can be handled via REST" in {
    // default rates
    node.getRates shouldBe defaultRateMap

    // add rate for wct asset
    node.upsertRate(wctAsset, wctRate, expectedStatusCode = Created).message shouldBe s"Rate $wctRate for the asset $wctStr added"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRate)

    // update rate for wct asset
    node.upsertRate(wctAsset, wctRateUpdated, expectedStatusCode = OK).message shouldBe
      s"Rate for the asset $wctStr updated, old value = $wctRate, new value = $wctRateUpdated"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // update rate for Waves is not allowed
    node.upsertRate(Waves, wctRateUpdated, expectedStatusCode = BadRequest).message shouldBe "Rate for Waves cannot be changed"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRateUpdated)

    // delete rate for wct asset
    node.deleteRate(wctAsset).message shouldBe s"Rate for asset $wctAsset removed"
    node.getRates shouldBe defaultRateMap
  }

  "Rates are restored from the DB after matcher's restart" in {
    // add rate for wct asset
    node.upsertRate(wctAsset, wctRate, expectedStatusCode = Created).message shouldBe s"Rate $wctRate for the asset $wctStr added"
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRate)

    // restart matcher
    docker.killAndStartContainer(node)

    // rates should be restored from DB
    node.getRates shouldBe defaultRateMap + (wctAsset -> wctRate)
  }
}
