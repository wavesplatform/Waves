package com.wavesplatform.http

import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.state2.{LeaseInfo, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{BlockGen, TestWallet, TransactionGen}
import org.scalacheck.{Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.History
import scorex.waves.http.DebugApiRoute

class DebugRouteSpec
  extends RouteSpec("/debug")
    with RestAPISettingsHelper with TestWallet with MockFactory with PropertyChecks with TransactionGen with BlockGen {

  private val state = mock[StateReader]
  private val history = mock[History]
  private val route = DebugApiRoute(restAPISettings, testWallet, state, history).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  
  routePath("/state") in {
    val portfolioGen = for {
      a <- accountGen
      b <- Gen.posNum[Long]
    } yield a.toAccount -> Portfolio(b, LeaseInfo.empty, Map.empty)

    forAll(Gen.chooseNum(0, 20).flatMap(n => Gen.listOfN(n, portfolioGen))) { portfolios =>
      val portfolioMap = portfolios.toMap
      (state.accountPortfolios _).expects().returning(portfolioMap).once()

      Get(routePath("/state")) ~> route ~> check {
        responseAs[JsObject] shouldEqual JsObject(portfolios.map {
          case (account, p) => account.address -> JsNumber(p.balance)
        })
      }
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
