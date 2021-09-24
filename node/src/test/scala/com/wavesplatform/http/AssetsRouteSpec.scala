package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.google.protobuf.ByteString
import com.wavesplatform.{RequestGen, TestTime, TestValues}
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi}
import com.wavesplatform.api.common.CommonAssetsApi.AssetInfo
import com.wavesplatform.api.http.ApiMarshallers._
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.api.http.requests.{TransferV1Request, TransferV2Request}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, Blockchain, Height}
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet
import monix.reactive.Observable
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.{JsObject, Json, JsValue, Writes}

class AssetsRouteSpec
    extends RouteSpec("/assets")
    with RequestGen
    with PathMockFactory
    with Eventually
    with RestAPISettingsHelper
    with PropertyChecks {

  private val wallet     = stub[Wallet]
  private val blockchain = stub[Blockchain]

  private val seed               = "seed".getBytes("UTF-8")
  private val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
  private val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

  (wallet.privateKeyAccount _).when(senderPrivateKey.toAddress).onCall((_: Address) => Right(senderPrivateKey)).anyNumberOfTimes()

  private val assetsApi: CommonAssetsApi = stub[CommonAssetsApi]
  private val accountsApi: CommonAccountsApi = stub[CommonAccountsApi]

  private val MaxDistributionDepth = 1
  private val route: Route = AssetsApiRoute(
    restAPISettings,
    wallet,
    DummyTransactionPublisher.accepting,
    blockchain,
    new TestTime(),
    accountsApi,
    assetsApi,
    MaxDistributionDepth
  ).route

  "/balance/{address}" - {
    "multiple ids" in {
      (blockchain.balance _).when(TxHelpers.defaultAddress, *).returning(100)
      (accountsApi.portfolio _).when(TxHelpers.defaultAddress).returning(Observable(IssuedAsset(ByteStr.decodeBase58("xxx").get) -> 999L))
      (assetsApi.fullInfo _).when(*).returning {
        val desc = AssetDescription(ByteStr.empty, TxHelpers.defaultSigner.publicKey, ByteString.EMPTY, ByteString.EMPTY, 0, false, BigInt(123), Height(123), None, 0L, nft = false)
        Some(AssetInfo(desc, None, None))
      }

      route.anyParamTest(routePath(s"/balance/${TxHelpers.defaultAddress}"), "id")("aaa", "bbb") {
        status shouldBe StatusCodes.OK
        responseAs[JsValue] should matchJson(
          """
            |{
            |  "address" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
            |  "balances" : [ {
            |    "assetId" : "aaa",
            |    "reissuable" : false,
            |    "minSponsoredAssetFee" : null,
            |    "sponsorBalance" : null,
            |    "quantity" : 123,
            |    "issueTransaction" : null,
            |    "balance" : 100
            |  }, {
            |    "assetId" : "bbb",
            |    "reissuable" : false,
            |    "minSponsoredAssetFee" : null,
            |    "sponsorBalance" : null,
            |    "quantity" : 123,
            |    "issueTransaction" : null,
            |    "balance" : 100
            |  } ]
            |}
            |""".stripMargin)
      }

      route.anyParamTest(routePath(s"/balance/${TxHelpers.defaultAddress}"), "id")("____", "----") {
        status shouldBe StatusCodes.BadRequest
        responseAs[JsValue] should matchJson("""{
                                               |    "error": 116,
                                               |    "message": "Request contains invalid IDs. ____, ----",
                                               |    "ids": [
                                               |        "____",
                                               |        "----"
                                               |    ]
                                               |}""".stripMargin)
      }

      withClue("over limit")(route.anyParamTest(routePath(s"/balance/${TxHelpers.defaultAddress}"), "id")(Seq.fill(101)("aaa"):_*) {
        status shouldBe StatusCodes.BadRequest
        responseAs[JsValue] should matchJson("""{
                                               |  "error" : 10,
                                               |  "message" : "Too big sequence requested: max limit is 100 entries"
                                               |}""".stripMargin)
      })

      withClue("old GET portfolio")(Get(routePath(s"/balance/${TxHelpers.defaultAddress}")) ~> route ~> check { // portfolio
        status shouldBe StatusCodes.OK
        responseAs[JsValue] should matchJson("""{
                                               |  "address" : "3MtGzgmNa5fMjGCcPi5nqMTdtZkfojyWHL9",
                                               |  "balances" : [ {
                                               |    "assetId" : "xxx",
                                               |    "balance" : 999,
                                               |    "reissuable" : false,
                                               |    "minSponsoredAssetFee" : null,
                                               |    "sponsorBalance" : null,
                                               |    "quantity" : 123,
                                               |    "issueTransaction" : null
                                               |  } ]
                                               |}""".stripMargin)
      })
    }
  }

  "/transfer" - {
    def posting[A: Writes](v: A): RouteTestResult = Post(routePath("/transfer"), v).addHeader(ApiKeyHeader) ~> route

    "accepts TransferRequest" in {
      val req = TransferV1Request(
        assetId = None,
        feeAssetId = None,
        amount = 1.waves,
        fee = 0.3.waves,
        sender = senderPrivateKey.toAddress.toString,
        attachment = Some("attachment"),
        recipient = receiverPrivateKey.toAddress.toString,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK

        responseAs[TransferTransaction]
      }
    }

    "accepts VersionedTransferRequest" in {
      val req = TransferV2Request(
        assetId = None,
        amount = 1.waves,
        feeAssetId = None,
        fee = 0.3.waves,
        sender = senderPrivateKey.toAddress.toString,
        attachment = None,
        recipient = receiverPrivateKey.toAddress.toString,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferV2Request]
      }
    }

    "returns a error if it is not a transfer request" in {
      val req = issueReq.sample.get
      posting(req) ~> check {
        status shouldNot be(StatusCodes.OK)
      }
    }
  }

  private val smartIssueAndDetailsGen = for {
    script       <- scriptGen
    smartAssetTx <- issueV2TransactionGen(_scriptGen = Gen.const(Some(script)))
  } yield (
    smartAssetTx,
    AssetDescription(
      originTransactionId = smartAssetTx.id(),
      issuer = smartAssetTx.sender,
      name = smartAssetTx.name,
      description = smartAssetTx.description,
      decimals = smartAssetTx.decimals,
      reissuable = smartAssetTx.reissuable,
      totalVolume = smartAssetTx.quantity,
      lastUpdatedAt = Height @@ 0,
      script = Some(AssetScriptInfo(script, Script.estimate(script, ScriptEstimatorV1, useContractVerifierLimit = false).explicitGet())),
      sponsorship = 0,
      nft = smartAssetTx.decimals == 0 && smartAssetTx.quantity == 1 && !smartAssetTx.reissuable
    )
  )

  routePath(s"/details/{id} - smart asset") in forAll(smartIssueAndDetailsGen) {
    case (smartAssetTx, smartAssetDesc) =>
      (blockchain.transactionInfo _).when(smartAssetTx.id()).onCall((_: ByteStr) => Some((1, smartAssetTx, true)))
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

  routePath("/{assetId}/distribution/{height}/limit/{limit}") in {
    (() => blockchain.height).when().returning(3)
    (assetsApi.assetDistribution _).when(TestValues.asset, *, *).returning(Observable(TestValues.address -> 10L))

    Get(routePath(s"/${TestValues.asset.id}/distribution/2/limit/$MaxAddressesPerRequest")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe Json.obj(
        "hasNext"  -> false,
        "lastItem" -> TestValues.address.stringRepr,
        "items" -> Json.obj(
          TestValues.address.stringRepr -> 10L
        )
      )
    }

    Get(routePath(s"/${TestValues.asset.id}/distribution/2/limit/${MaxAddressesPerRequest + 1}")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj("error" -> 199, "message" -> s"Limit should be less than or equal to $MaxAddressesPerRequest")
    }

    Get(routePath(s"/${TestValues.asset.id}/distribution/1/limit/1")) ~> route ~> check {
      responseAs[JsObject] shouldBe Json.obj(
        "error"   -> 199,
        "message" -> s"Unable to get distribution past height ${blockchain.height - MaxDistributionDepth}"
      )
    }
  }

  private val sillyIssueAndDetailsGen = for {
    sillyAssetTx <- issueV2TransactionGen(_scriptGen = Gen.const(None))
  } yield (
    sillyAssetTx,
    AssetDescription(
      originTransactionId = sillyAssetTx.id(),
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

  routePath(s"/details/{id} - non-smart asset") in forAll(sillyIssueAndDetailsGen) {
    case (sillyAssetTx, sillyAssetDesc) =>
      (blockchain.transactionInfo _).when(sillyAssetTx.id()).onCall((_: ByteStr) => Some((1, sillyAssetTx, true)))
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
    (response \ "issuer").as[String] shouldBe tx.sender.toAddress.toString
    (response \ "name").as[String] shouldBe tx.name.toStringUtf8
    (response \ "description").as[String] shouldBe tx.description.toStringUtf8
    (response \ "decimals").as[Int] shouldBe tx.decimals
    (response \ "reissuable").as[Boolean] shouldBe tx.reissuable
    (response \ "quantity").as[BigDecimal] shouldBe desc.totalVolume
    (response \ "minSponsoredAssetFee").asOpt[Long] shouldBe empty
    (response \ "originTransactionId").as[String] shouldBe tx.id().toString
  }
}
