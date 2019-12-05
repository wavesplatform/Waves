package com.wavesplatform.api.http

import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.http.{RestAPISettingsHelper, RouteSpec}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.state.{AssetDescription, Blockchain, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.{NoShrink, TestTime, TestWallet, TransactionGen}
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._

class AssetsApiRouteSpec
    extends RouteSpec("/assets")
    with PathMockFactory
    with PropertyChecks
    with RestAPISettingsHelper
    with TestWallet
    with TransactionGen
    with NoShrink {

  private val blockchain = stub[Blockchain]

  private val route =
    AssetsApiRoute(restAPISettings, testWallet, mock[UtxPoolSynchronizer], blockchain, new TestTime).route

  private val smartAssetTx = issueV2TransactionGen().retryUntil(_.script.nonEmpty).sample.get
  private val smartAssetDesc = AssetDescription(
    issuer = smartAssetTx.sender,
    name = Left(smartAssetTx.nameBytes),
    description = Left(smartAssetTx.descriptionBytes),
    decimals = smartAssetTx.decimals,
    reissuable = smartAssetTx.reissuable,
    totalVolume = smartAssetTx.quantity,
    lastUpdatedAt = Height @@ 0,
    script = smartAssetTx.script,
    sponsorship = 0
  )

  (blockchain.transactionInfo _).when(smartAssetTx.id()).onCall((_: ByteStr) => Some((1, smartAssetTx)))
  (blockchain.assetDescription _).when(IssuedAsset(smartAssetTx.id())).onCall((_: IssuedAsset) => Some(smartAssetDesc))
  routePath(s"/details/${smartAssetTx.id().toString}") in {
    Get(routePath(s"/details/${smartAssetTx.id().toString}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "assetId").as[String] shouldBe smartAssetTx.id().toString
      (response \ "issueHeight").as[Long] shouldBe 1
      (response \ "issueTimestamp").as[Long] shouldBe smartAssetTx.timestamp
      (response \ "issuer").as[String] shouldBe smartAssetTx.sender.stringRepr
      (response \ "name").as[String] shouldBe smartAssetTx.name
      (response \ "description").as[String] shouldBe smartAssetTx.description
      (response \ "decimals").as[Int] shouldBe smartAssetTx.decimals
      (response \ "reissuable").as[Boolean] shouldBe smartAssetTx.reissuable
      (response \ "quantity").as[BigDecimal] shouldBe smartAssetDesc.totalVolume
      (response \ "minSponsoredAssetFee").asOpt[Long] shouldBe empty
    }
  }

  private val sillyAssetTx = issueGen.sample.get
  private val sillyAssetDesc = AssetDescription(
    issuer = sillyAssetTx.sender,
    name = Left(sillyAssetTx.nameBytes),
    description = Left(sillyAssetTx.descriptionBytes),
    decimals = sillyAssetTx.decimals,
    reissuable = sillyAssetTx.reissuable,
    totalVolume = sillyAssetTx.quantity,
    lastUpdatedAt = Height @@ 0,
    script = sillyAssetTx.script,
    sponsorship = 0
  )
  (blockchain.transactionInfo _).when(sillyAssetTx.id()).onCall((_: ByteStr) => Some((1, sillyAssetTx)))
  (blockchain.assetDescription _).when(IssuedAsset(sillyAssetTx.id())).onCall((_: IssuedAsset) => Some(sillyAssetDesc))
  routePath(s"/details/${sillyAssetTx.id().toString}") in {
    Get(routePath(s"/details/${sillyAssetTx.id().toString}")) ~> route ~> check {
      val response = responseAs[JsObject]
      (response \ "assetId").as[String] shouldBe sillyAssetTx.id().toString
      (response \ "issueHeight").as[Long] shouldBe 1
      (response \ "issueTimestamp").as[Long] shouldBe sillyAssetTx.timestamp
      (response \ "issuer").as[String] shouldBe sillyAssetTx.sender.stringRepr
      (response \ "name").as[String] shouldBe sillyAssetTx.name
      (response \ "description").as[String] shouldBe sillyAssetTx.description
      (response \ "decimals").as[Int] shouldBe sillyAssetTx.decimals
      (response \ "reissuable").as[Boolean] shouldBe sillyAssetTx.reissuable
      (response \ "quantity").as[BigDecimal] shouldBe sillyAssetDesc.totalVolume
      (response \ "minSponsoredAssetFee").asOpt[Long] shouldBe empty
    }
  }
}
