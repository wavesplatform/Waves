package com.wavesplatform.http

import com.wavesplatform.TransactionGen
import com.wavesplatform.http.ApiMarshallers._
import org.scalacheck.{Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.api.http.{InvalidSignature, TransactionsApiRoute}
import scorex.crypto.encode.Base58
import scorex.transaction._

class TransactionsRouteSpec extends RouteSpec("/transactions")
  with RestAPISettingsHelper with MockFactory with Matchers with TransactionGen with PropertyChecks {

  private val history = mock[History]
  private val state = mock[State]
  private val stm = mock[TransactionModule]
  private val route = TransactionsApiRoute(restAPISettings, state, history, stm).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  routePath("/address/{address}/limit/{limit}") in {
    // todo: check invalid address
    // todo: check invalid limit

  }

  routePath("/info/{signature}") in {
    forAll(Gen.alphaNumStr.map(_ + "O")) { invalidBase58 =>
      Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidSignature)
    }

    Get(routePath("/info/")) ~> route should produce(InvalidSignature)

    val txAvailability = for {
      tx <- randomTransactionGen
      height <- Gen.option(Gen.posNum[Int])
    } yield (tx, height)

    def sameSignature(target: Array[Byte])(actual: Array[Byte]): Boolean = target sameElements actual

    forAll(txAvailability) {
      case (tx, height) =>
        (state.included _).expects(where(sameSignature(tx.signature) _)).returning(height).once()
        height.foreach { h => (history.blockAt _).expects(h).returning(None).once() }
        val result = Get(routePath(s"/info/${Base58.encode(tx.signature)}")) ~> route ~> runRoute
    }
  }

  routePath("/unconfirmed") in {
    val g = for {
      i <- Gen.chooseNum(0, 20)
      t <- Gen.listOfN(i, randomTransactionGen)
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
