package com.wavesplatform.api.http

import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.http.{RestAPISettingsHelper, RouteSpec}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.state.{AssetDescription, Blockchain, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.{NoShrink, TestTime, TestWallet, TransactionGen}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json._
import com.wavesplatform.common.utils.EitherExt2

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
    AssetsApiRoute(restAPISettings, testWallet, mock[UtxPoolSynchronizer], blockchain, new TestTime, mock[CommonAccountsApi], mock[CommonAssetsApi]).route

  private val smartIssueAndDetailsGen = for {
    script       <- scriptGen
    smartAssetTx <- issueV2TransactionGen(_scriptGen = Gen.const(Some(script)))
  } yield (
    smartAssetTx,
    AssetDescription(
      source = smartAssetTx.id(),
      issuer = smartAssetTx.sender,
      name = smartAssetTx.name,
      description = smartAssetTx.description,
      decimals = smartAssetTx.decimals,
      reissuable = smartAssetTx.reissuable,
      totalVolume = smartAssetTx.quantity,
      lastUpdatedAt = Height @@ 0,
      script = Some((script, Script.estimate(script, ScriptEstimatorV1).explicitGet())),
      sponsorship = 0,
      nft = smartAssetTx.decimals == 0 && smartAssetTx.quantity == 1 && !smartAssetTx.reissuable
    )
  )

  routePath(s"/details/{id}") in forAll(smartIssueAndDetailsGen) {
    case (smartAssetTx, smartAssetDesc) =>
      (blockchain.transactionInfo _).when(smartAssetTx.id()).onCall((_: ByteStr) => Some((1, smartAssetTx)))
      (blockchain.assetDescription _).when(IssuedAsset(smartAssetTx.id())).onCall((_: IssuedAsset) => Some(smartAssetDesc))

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

  private val sillyIssueAndDetailsGen = for {
    sillyAssetTx <- issueV2TransactionGen(_scriptGen = Gen.const(None))
  } yield (
    sillyAssetTx,
    AssetDescription(
      source = sillyAssetTx.id(),
      issuer = sillyAssetTx.sender,
      name = sillyAssetTx.name,
      description = sillyAssetTx.description,
      decimals = sillyAssetTx.decimals,
      reissuable = sillyAssetTx.reissuable,
      totalVolume = sillyAssetTx.quantity,
      lastUpdatedAt = Height @@ 0,
      script = None,
      sponsorship = 0,
      nft = sillyAssetTx.decimals == 0 && sillyAssetTx.quantity == 1 && !sillyAssetTx.reissuable
    )
  )

  routePath(s"/details/{id}") in forAll(sillyIssueAndDetailsGen) {
    case (sillyAssetTx, sillyAssetDesc) =>
      (blockchain.transactionInfo _).when(sillyAssetTx.id()).onCall((_: ByteStr) => Some((1, sillyAssetTx)))
      (blockchain.assetDescription _).when(IssuedAsset(sillyAssetTx.id())).onCall((_: IssuedAsset) => Some(sillyAssetDesc))
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
