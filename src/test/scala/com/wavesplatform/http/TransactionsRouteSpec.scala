package com.wavesplatform.http

import com.wavesplatform.http.ApiMarshallers._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.api.http.TransactionsApiRoute
import scorex.crypto.encode.Base58
import scorex.transaction._

class TransactionsRouteSpec extends RouteSpec("/transactions")
  with RestAPISettingsHelper with MockFactory with Matchers with TransactionGen with PropertyChecks {

  private val history = mock[History]
  private val state = mock[State]
  private val stm = mock[TransactionModule]
  private val route = TransactionsApiRoute(restAPISettings, state, history, stm).route

  private val txGen = for {
    tr <- transferGenerator
    is <- issueGenerator
    al <- createAliasGenerator
    le <- leaseGenerator
    cl <- leaseCancelGenerator
    tx <- Gen.oneOf(tr, is, al, le, cl)
  } yield tx

  routePath("/address/{address}/limit/{limit}") in {
    // todo: check invalid address
    // todo: check invalid limit
  }
  routePath("/info/{signature}") in {
    // todo: check invalid signature
    // todo: check empty signature

  }

  routePath("/unconfirmed") in {
    val g = for {
      i <- Gen.chooseNum(0, 20)
      t <- Gen.listOfN(i, txGen)
    } yield t

    forAll(g) { txs =>
      (stm.unconfirmedTxs _).expects().returning(txs).once()
      Get(routePath("/unconfirmed")) ~> route ~> check {
        val resp = responseAs[Seq[JsValue]]
        for ((r, t) <- resp.zip(txs)) {
          (r \ "signature").as[String] shouldEqual Base58.encode(t.signature)
        }
      }
    }
  }
}
