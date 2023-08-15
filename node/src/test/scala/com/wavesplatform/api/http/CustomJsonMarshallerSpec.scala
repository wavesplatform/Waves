package com.wavesplatform.api.http

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.http.{ApiErrorMatchers, RestAPISettingsHelper}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.reader.{CompositeBlockchain, LeaseDetails}
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.SharedSchedulerMixin
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.{NTPTime, TestWallet}
import org.scalactic.source.Position
import org.scalamock.scalatest.PathMockFactory
import play.api.libs.json.*

import scala.concurrent.duration.DurationInt
import scala.reflect.ClassTag

class CustomJsonMarshallerSpec
    extends PropSpec
    with RestAPISettingsHelper
    with PathMockFactory
    with TestWallet
    with NTPTime
    with ScalatestRouteTest
    with ApiErrorMatchers
    with ApiMarshallers
    with SharedSchedulerMixin {
  private val blockchain      = mock[Blockchain]
  private val utx             = mock[UtxPool]
  private val publisher       = mock[TransactionPublisher]
  private val transactionsApi = mock[CommonTransactionsApi]
  private val accountsApi     = mock[CommonAccountsApi]
  private val assetsApi       = mock[CommonAssetsApi]

  private val numberFormat = Accept(`application/json`.withParams(Map("large-significand-format" -> "string")))

  (() => blockchain.activatedFeatures).expects().returning(BlockchainFeatures.implemented.map(_ -> 0).toMap).anyNumberOfTimes()
  (() => blockchain.settings).expects().returning(DefaultBlockchainSettings).anyNumberOfTimes()

  private def ensureFieldsAre[A: ClassTag](v: JsObject, fields: String*)(implicit pos: Position): Unit =
    for (f <- fields) (v \ f).get shouldBe a[A]

  private def checkRoute(req: HttpRequest, route: Route, fields: String*)(implicit pos: Position): Unit = {
    req ~> route ~> check {
      ensureFieldsAre[JsNumber](responseAs[JsObject], fields*)
    }

    req ~> numberFormat ~> route ~> check {
      ensureFieldsAre[JsString](responseAs[JsObject], fields*)
    }
  }

  private val transactionsRoute =
    TransactionsApiRoute(
      restAPISettings,
      transactionsApi,
      testWallet,
      blockchain,
      mock[() => CompositeBlockchain],
      () => utx.size,
      publisher,
      ntpTime,
      new RouteTimeout(60.seconds)(sharedScheduler)
    ).route

  property("/transactions/info/{id}") {
    forAll(leaseGen) { lt =>
      val height: Height = Height(1)
      (transactionsApi.transactionById _).expects(lt.id()).returning(Some(TransactionMeta.Default(height, lt, Status.Succeeded, 0L))).twice()
      (blockchain.leaseDetails _)
        .expects(lt.id())
        .returning(Some(LeaseDetails(lt.sender, lt.recipient, lt.amount.value, LeaseDetails.Status.Active, lt.id(), 1)))
        .twice()
      checkRoute(Get(s"/transactions/info/${lt.id()}"), transactionsRoute, "amount")
    }
  }

  property("/transactions/calculateFee") {
    (() => blockchain.height).expects().returning(1000).anyNumberOfTimes()
    (blockchain.assetScript _).expects(*).returning(None).anyNumberOfTimes()

    forAll(transferV2Gen) { tx =>
      (transactionsApi.calculateFee _).expects(*).returning(Right((Asset.Waves, 1, 1))).twice()
      checkRoute(Post("/transactions/calculateFee", tx.json()), transactionsRoute, "feeAmount")
    }
  }

  private val rewardRoute = RewardApiRoute(blockchain).route

  property("/blockchain/rewards") {
    (() => blockchain.height).expects().returning(1000).anyNumberOfTimes()
    (blockchain.blockReward _).expects(*).returning(Some(1000)).twice()
    (blockchain.wavesAmount _).expects(*).returning(BigInt(10000000)).twice()
    (blockchain.blockRewardVotes _).expects(1000).returning(Seq(100L)).twice()

    checkRoute(Get("/blockchain/rewards/1000"), rewardRoute, "totalWavesAmount", "currentReward", "minIncrement")
  }

  property("/debug/stateWaves") {
    pending // todo: fix when distributions/portfolio become testable
  }

  private val assetsRoute = AssetsApiRoute(
    restAPISettings,
    testWallet,
    publisher,
    blockchain,
    mock[() => CompositeBlockchain],
    ntpTime,
    accountsApi,
    assetsApi,
    1000,
    new RouteTimeout(60.seconds)(sharedScheduler)
  ).route

  property("/assets/{assetId}/distribution/{height}/limit/{limit}") {
    pending // todo: fix when distributions/portfolio become testable
  }

  property("/assets/balance/{address}/{assetId}") {
    forAll(accountGen, bytes32gen.map(b => IssuedAsset(ByteStr(b)))) { case (keyPair, assetId) =>
      (blockchain.balance _).expects(keyPair.toAddress, assetId).returning(1000L).twice()
      checkRoute(Get(s"/assets/balance/${keyPair.publicKey.toAddress}/${assetId.id}"), assetsRoute, "balance")
    }
  }
}
