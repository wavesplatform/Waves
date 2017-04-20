package com.wavesplatform.http

import com.wavesplatform.{TestWallet, TransactionGen}
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.state2.{LeaseInfo, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.transaction.History
import scorex.waves.http.DebugApiRoute

class DebugRouteSpec
  extends RouteSpec("/debug")
    with RestAPISettingsHelper with TestWallet with MockFactory with PropertyChecks with TransactionGen{

  private val state = mock[StateReader]
  private val history = mock[History]
  private val route = DebugApiRoute(restAPISettings, testWallet, state, history).route

  routePath("blocks/{howMany}") in {

  }

  routePath("/state") in {
    val portfolioGen = for {
      account <- accountGen
      balance <- Gen.posNum[Long]
    } yield account -> Portfolio(balance, LeaseInfo.empty, Map.empty)
    forAll(Gen.listOf()) { case (account, balance) =>

    }
  }

  routePath("/info") in {
    forAll(Gen.posNum[Int]) { height =>
      (state.height _).expects().returning(height).once()
      (state.accountPortfolios _).expects().returning(Map.empty).once()
      Get(routePath("/info")) ~> route ~> check {
        responseAs[JsObject] should have (
          "stateHeight" -> JsNumber(height),
          "stateHash".ofType[JsNumber]
        )
      }
    }
  }
}
