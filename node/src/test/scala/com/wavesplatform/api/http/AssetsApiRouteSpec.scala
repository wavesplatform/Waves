package com.wavesplatform.api.http

import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.http.{RestAPISettingsHelper, RouteSpec}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.state.{AssetDescription, Blockchain, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
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
    source = smartAssetTx.id(),
    issuer = smartAssetTx.sender,
    name = smartAssetTx.name,
    description = smartAssetTx.description,
    decimals = smartAssetTx.decimals,
    reissuable = smartAssetTx.reissuable,
    totalVolume = smartAssetTx.quantity,
    lastUpdatedAt = Height @@ 0,
    script = smartAssetTx.script,
    sponsorship = 0,
    nft = smartAssetTx.decimals == 0 && smartAssetTx.quantity == 1 && !smartAssetTx.reissuable
  )

  (blockchain.transactionInfo _).when(smartAssetTx.id()).onCall((_: ByteStr) => Some((1, smartAssetTx)))
  (blockchain.assetDescription _).when(IssuedAsset(smartAssetTx.id())).onCall((_: IssuedAsset) => Some(smartAssetDesc))
  routePath(s"/details/${smartAssetTx.id().toString}") in {
    Get(routePath(s"/details/${smartAssetTx.id().toString}")) ~> route ~> check {
      val response = responseAs[JsObject]
      checkResponse(smartAssetTx, smartAssetDesc, response)
    }
    Get(routePath(s"/details?id=${smartAssetTx.id().toString}")) ~> route ~> check {
      val responses = responseAs[List[JsObject]]
      responses.foreach(response => checkResponse(smartAssetTx, smartAssetDesc, response))
    }
    Post(routePath("/details"), Json.obj("ids" -> List(s"${smartAssetTx.id().toString}"))) ~> route ~> check {
      val responses = responseAs[List[JsObject]]
      responses.foreach(response => checkResponse(smartAssetTx, smartAssetDesc, response))
    }
  }

  private val sillyAssetTx = issueGen.sample.get
  private val sillyAssetDesc = AssetDescription(
    source = sillyAssetTx.id(),
    issuer = sillyAssetTx.sender,
    name = sillyAssetTx.name,
    description = sillyAssetTx.description,
    decimals = sillyAssetTx.decimals,
    reissuable = sillyAssetTx.reissuable,
    totalVolume = sillyAssetTx.quantity,
    lastUpdatedAt = Height @@ 0,
    script = sillyAssetTx.script,
    sponsorship = 0,
    nft = sillyAssetTx.decimals == 0 && sillyAssetTx.quantity == 1 && !sillyAssetTx.reissuable
  )
  (blockchain.transactionInfo _).when(sillyAssetTx.id()).onCall((_: ByteStr) => Some((1, sillyAssetTx)))
  (blockchain.assetDescription _).when(IssuedAsset(sillyAssetTx.id())).onCall((_: IssuedAsset) => Some(sillyAssetDesc))
  routePath(s"/details/${sillyAssetTx.id().toString}") in {
    Get(routePath(s"/details/${sillyAssetTx.id().toString}")) ~> route ~> check {
      val response = responseAs[JsObject]
      checkResponse(sillyAssetTx, sillyAssetDesc, response)
    }
    Get(routePath(s"/details?id=${sillyAssetTx.id().toString}")) ~> route ~> check {
      val responses = responseAs[List[JsObject]]
      responses.foreach(response => checkResponse(sillyAssetTx, sillyAssetDesc, response))
    }
    Post(routePath("/details"), Json.obj("ids" -> List(s"${sillyAssetTx.id().toString}"))) ~> route ~> check {
      val responses = responseAs[List[JsObject]]
      responses.foreach(response => checkResponse(sillyAssetTx, sillyAssetDesc, response))
    }
  }

  private def checkResponse(tx: IssueTransaction, desc: AssetDescription, response: JsObject): Unit = {
    (response \ "assetId").as[String] shouldBe tx.id().toString
    (response \ "issueHeight").as[Long] shouldBe 1
    (response \ "issueTimestamp").as[Long] shouldBe tx.timestamp
    (response \ "issuer").as[String] shouldBe tx.sender.stringRepr
    (response \ "name").as[String] shouldBe tx.name.toStringUtf8
    (response \ "description").as[String] shouldBe tx.description.toStringUtf8
    (response \ "decimals").as[Int] shouldBe tx.decimals
    (response \ "reissuable").as[Boolean] shouldBe tx.reissuable
    (response \ "quantity").as[BigDecimal] shouldBe desc.totalVolume
    (response \ "minSponsoredAssetFee").asOpt[Long] shouldBe empty
    (response \ "originTransactionId").as[String] shouldBe tx.id().toString
  }
}
