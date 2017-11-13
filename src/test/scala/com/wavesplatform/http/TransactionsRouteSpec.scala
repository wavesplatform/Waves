package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.{BlockGen, NoShrink, TransactionGen, UtxPool}
import monix.eval.Coeval
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.account.Address
import scorex.api.http.{InvalidAddress, InvalidSignature, TooBigArrayAllocation, TransactionsApiRoute}
import scorex.crypto.encode.Base58
import scorex.transaction._

class TransactionsRouteSpec extends RouteSpec("/transactions")
  with RestAPISettingsHelper
  with MockFactory
  with Matchers
  with TransactionGen
  with BlockGen
  with PropertyChecks
  with NoShrink {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  private val transactionsCount = 10

  private val history = mock[History]
  private val state = mock[SnapshotStateReader]
  private val utx = mock[UtxPool]
  private val route = TransactionsApiRoute(restAPISettings, Coeval.now(state), history, utx).route


  routePath("/address/{address}/limit/{limit}") - {
    "handles invalid address" in {
      forAll(bytes32gen, choose(1, MaxTransactionsPerRequest)) { case (bytes, limit) =>
        Get(routePath(s"/address/${Base58.encode(bytes)}/limit/$limit")) ~> route should produce(InvalidAddress)
      }
    }

    "handles invalid limit" in {
      forAll(accountGen, alphaStr.label("alphaNumericLimit")) { case (account, invalidLimit) =>
        Get(routePath(s"/address/${account.address}/limit/$invalidLimit")) ~> route ~> check {
          status shouldEqual StatusCodes.BadRequest
          (responseAs[JsObject] \ "message").as[String] shouldEqual "invalid.limit"
        }
      }

      forAll(accountGen, choose(MaxTransactionsPerRequest + 1, Int.MaxValue).label("limitExceeded")) { case (account, limit) =>
        Get(routePath(s"/address/${account.address}/limit/$limit")) ~> route should produce(TooBigArrayAllocation)
      }
    }

    "working properly otherwise" ignore {
      forAll(
        accountGen,
        choose(1, MaxTransactionsPerRequest),
        randomTransactionsGen(transactionsCount)) { case (account, limit, txs) =>
        (state.accountTransactionIds _).expects(account: Address, limit).returning(txs.map(_.id())).once()
        txs.foreach { tx =>
          (state.transactionInfo _).expects(tx.id()).returning(Some((1, tx))).once()
        }
        Get(routePath(s"/address/${account.address}/limit/$limit")) ~> route ~> check {
          responseAs[Seq[JsValue]].length shouldEqual txs.length.min(limit)
        }
      }
    }
  }

  routePath("/info/{signature}") - {
    "handles invalid signature" in {
      forAll(alphaNumStr.map(_ + "O")) { invalidBase58 =>
        Get(routePath(s"/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      val txAvailability = for {
        tx <- randomTransactionGen
        height <- posNum[Int]
      } yield (tx, height)

      forAll(txAvailability) { case (tx, height) =>
        (state.transactionInfo _).expects(tx.id()).returning(Some((height, tx))).once()
        Get(routePath(s"/info/${tx.id().base58}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual tx.json() + ("height" -> JsNumber(height))
        }
      }
    }
  }

  routePath("/unconfirmed") - {
    "returns the list of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.all _).expects().returns(txs).once()
        Get(routePath("/unconfirmed")) ~> route ~> check {
          val resp = responseAs[Seq[JsValue]]
          for ((r, t) <- resp.zip(txs)) {
            (r \ "signature").as[String] shouldEqual t.signature.base58
          }
        }
      }
    }
  }

  routePath("/unconfirmed/size") - {
    "returns the size of unconfirmed transactions" in {
      val g = for {
        i <- chooseNum(0, 20)
        t <- listOfN(i, randomTransactionGen)
      } yield t

      forAll(g) { txs =>
        (utx.size _).expects().returns(txs.size).once()
        Get(routePath("/unconfirmed/size")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual Json.obj("size" -> JsNumber(txs.size))
        }
      }
    }
  }

  routePath("/unconfirmed/info/{signature}") - {
    "handles invalid signature" in {
      forAll(alphaNumStr.map(_ + "O")) { invalidBase58 =>
        Get(routePath(s"/unconfirmed/info/$invalidBase58")) ~> route should produce(InvalidSignature)
      }

      Get(routePath(s"/unconfirmed/info/")) ~> route should produce(InvalidSignature)
      Get(routePath(s"/unconfirmed/info")) ~> route should produce(InvalidSignature)
    }

    "working properly otherwise" in {
      forAll(randomTransactionGen) { tx =>
        (utx.transactionById _).expects(tx.id()).returns(Some(tx)).once()
        Get(routePath(s"/unconfirmed/info/${tx.id().base58}")) ~> route ~> check {
          status shouldEqual StatusCodes.OK
          responseAs[JsValue] shouldEqual tx.json()
        }
      }
    }
  }
}
