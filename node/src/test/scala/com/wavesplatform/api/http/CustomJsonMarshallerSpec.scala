package com.wavesplatform.api.http

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.wavesplatform.api.http.assets.AssetsApiRoute
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.DefaultBlockchainSettings
import com.wavesplatform.http.{ApiErrorMatchers, RestAPISettingsHelper}
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.{NTPTime, NoShrink, TestWallet, TransactionGen}
import org.scalactic.source.Position
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._

import scala.reflect.ClassTag

class CustomJsonMarshallerSpec
    extends PropSpec
    with RestAPISettingsHelper
    with PathMockFactory
    with TestWallet
    with NTPTime
    with ScalatestRouteTest
    with Matchers
    with ApiErrorMatchers
    with NoShrink
    with ScalaCheckPropertyChecks
    with TransactionGen {
  private val blockchain      = mock[Blockchain]
  private val utx             = mock[UtxPool]
  private val utxSynchronizer = mock[UtxPoolSynchronizer]

  private val numberFormat = Accept(`application/json`.withParams(Map("large-significand-format" -> "string")))

  (blockchain.activatedFeatures _).expects().returning(BlockchainFeatures.implemented.map(_ -> 0).toMap).anyNumberOfTimes()
  (blockchain.settings _).expects().returning(DefaultBlockchainSettings).anyNumberOfTimes()

  private def ensureFieldsAre[A: ClassTag](v: JsObject, fields: String*)(implicit pos: Position): Unit =
    for (f <- fields) (v \ f).get shouldBe a[A]

  private def checkRoute(req: HttpRequest, route: Route, fields: String*)(implicit pos: Position): Unit = {
    req ~> route ~> check {
      ensureFieldsAre[JsNumber](responseAs[JsObject], fields: _*)
    }

    req ~> numberFormat ~> route ~> check {
      ensureFieldsAre[JsString](responseAs[JsObject], fields: _*)
    }
  }

  private val transactionsRoute = TransactionsApiRoute(restAPISettings, testWallet, blockchain, utx, utxSynchronizer, ntpTime).route

  property("/transactions/info/{id}") {
    forAll(leaseGen) { lt =>
      (blockchain.transactionInfo _).expects(lt.id()).returning(Some(1 -> lt)).twice()
      (blockchain.leaseDetails _)
        .expects(lt.id())
        .returning(Some(LeaseDetails(lt.sender, lt.recipient, 1, lt.amount, true)))
        .twice()
      checkRoute(Get(s"/transactions/info/${lt.id()}"), transactionsRoute, "amount")
    }
  }

  property("/transactions/calculateFee") {
    (blockchain.height _).expects().returning(1000).anyNumberOfTimes()
    (blockchain.hasAssetScript _).expects(*).returning(false).anyNumberOfTimes()
    (blockchain.hasScript _).expects(*).returning(false).anyNumberOfTimes()
    forAll(randomTransactionGen) { tx =>
      checkRoute(Post("/transactions/calculateFee", tx.json()), transactionsRoute, "feeAmount")
    }
  }

  private val rewardRoute = RewardApiRoute(blockchain).route

  property("/blockchain/rewards") {
    (blockchain.height _).expects().returning(1000).anyNumberOfTimes()
    (blockchain.blockReward _).expects(*).returning(Some(1000)).twice()
    (blockchain.wavesAmount _).expects(*).returning(BigInt(10000000)).twice()
    (blockchain.blockRewardVotes _).expects(1000).returning(Seq(100L)).twice()

    checkRoute(Get("/blockchain/rewards/1000"), rewardRoute, "totalWavesAmount", "currentReward", "minIncrement")
  }

  property("/debug/stateWaves") {
    pending // todo: fix when distributions/portfolio become testable
  }

  private val assetsRoute = AssetsApiRoute(restAPISettings, testWallet, utxSynchronizer, blockchain, ntpTime).route

  property("/assets/{assetId}/distribution/{height}/limit/{limit}") {
    pending // todo: fix when distributions/portfolio become testable
  }

  property("/assets/balance/{address}/{assetId}") {
    forAll(accountGen, bytes32gen.map(b => IssuedAsset(ByteStr(b)))) {
      case (keyPair, assetId) =>
        (blockchain.balance _).expects(keyPair.toAddress, assetId).returning(1000L).twice()
        checkRoute(Get(s"/assets/balance/${keyPair.publicKey.toAddress}/${assetId.id}"), assetsRoute, "balance")
    }
  }
}
