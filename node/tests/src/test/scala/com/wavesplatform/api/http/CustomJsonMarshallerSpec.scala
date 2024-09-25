package com.wavesplatform.api.http

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.http.{ApiErrorMatchers, DummyTransactionPublisher, RestAPISettingsHelper}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.SharedSchedulerMixin
import org.scalactic.source.Position
import play.api.libs.json.*

import scala.concurrent.duration.DurationInt
import scala.reflect.ClassTag

class CustomJsonMarshallerSpec
    extends PropSpec
    with RestAPISettingsHelper
    with ScalatestRouteTest
    with ApiErrorMatchers
    with ApiMarshallers
    with SharedDomain
    with SharedSchedulerMixin {

  private val numberFormat = Accept(`application/json`.withParams(Map("large-significand-format" -> "string")))
  private val richAccount  = TxHelpers.signer(55)

  override def genesisBalances: Seq[AddrWithBalance] = Seq(AddrWithBalance(richAccount.toAddress, 50000.waves))
  override def settings: WavesSettings               = DomainPresets.BlockRewardDistribution

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
      domain.transactionsApi,
      domain.wallet,
      domain.blockchain,
      () => domain.blockchain,
      () => domain.utxPool.size,
      DummyTransactionPublisher.accepting,
      ntpTime,
      new RouteTimeout(60.seconds)(sharedScheduler)
    ).route

  property("/transactions/info/{id}") {
    // todo: add other transaction types
    val leaseTx = TxHelpers.lease(sender = richAccount, TxHelpers.address(80), 25.waves)
    domain.appendBlock(leaseTx)
    checkRoute(Get(s"/transactions/info/${leaseTx.id()}"), transactionsRoute, "amount")
  }

  property("/transactions/calculateFee") {
    val tx = TxHelpers.transfer(richAccount, TxHelpers.address(81), 5.waves)
    checkRoute(Post("/transactions/calculateFee", tx.json()), transactionsRoute, "feeAmount")
  }

  private val rewardRoute = RewardApiRoute(domain.blockchain).route

  property("/blockchain/rewards") {
    checkRoute(Get("/blockchain/rewards/2"), rewardRoute, "totalWavesAmount", "currentReward", "minIncrement")
  }

  property("/debug/stateWaves") {
    pending // todo: fix when distributions/portfolio become testable
  }

  private val assetsRoute = AssetsApiRoute(
    restAPISettings,
    60.seconds,
    domain.wallet,
    domain.blockchain,
    () => domain.blockchain,
    ntpTime,
    domain.accountsApi,
    domain.assetsApi,
    1000,
    new RouteTimeout(60.seconds)(sharedScheduler)
  ).route

  property("/assets/{assetId}/distribution/{height}/limit/{limit}") {
    pending // todo: fix when distributions/portfolio become testable
  }

  property("/assets/balance/{address}/{assetId}") {
    val issue = TxHelpers.issue(richAccount, 100000_00, 2.toByte)
    domain.appendBlock(issue)
    checkRoute(Get(s"/assets/balance/${richAccount.toAddress}/${issue.id()}"), assetsRoute, "balance")
  }
}
