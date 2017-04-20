package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.state2.EqByteArray
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.{BlockGen, TransactionGen}
import org.scalacheck.Gen._
import org.scalacheck.Shrink
import org.scalamock.scalatest.MockFactory
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.account.Account
import scorex.api.http.{InvalidAddress, InvalidSignature, TooBigArrayAllocation, TransactionsApiRoute}
import scorex.crypto.encode.Base58
import scorex.transaction._

import scala.util.Random

class TransactionsRouteSpec extends RouteSpec("/transactions")
  with RestAPISettingsHelper with MockFactory with Matchers with TransactionGen with BlockGen with PropertyChecks {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  private val history = mock[History]
  private val state = mock[StateReader]
  private val stm = mock[TransactionModule]
  private val route = TransactionsApiRoute(restAPISettings, state, history, stm).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

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

    "working properly otherwise" in {
      forAll(
        accountGen,
        choose(1, MaxTransactionsPerRequest),
        listOfN(10, randomTransactionGen)) { case (account, limit, txs) =>
        (state.accountTransactionIds _).expects(account: Account).returning(txs.map(_.id).map(EqByteArray)).once()
        txs.foreach { tx =>
          (state.transactionInfo _).expects(EqByteArray(tx.id)).returning(Some(1,tx)).once()
        }
        Get(routePath(s"/address/${account.address}/limit/$limit")) ~> route ~> check {
          responseAs[Seq[JsValue]].length shouldEqual txs.length
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
        txList <- listOfN(99, tx)
        signer <- accountGen
        blk <- blockGen(Random.shuffle(tx :: txList), signer)
        height <- option(posNum[Int])
      } yield (tx, height, blk)

      forAll(txAvailability) { case (tx, height, block) =>
        (state.transactionInfo _).expects(EqByteArray(tx.id)).returning(height.map((_, tx))).once()
        height.foreach { h => (history.blockAt _).expects(h).returning(Some(block)).once() }
        Get(routePath(s"/info/${Base58.encode(tx.id)}")) ~> route ~> check {
          height match {
            case None => status shouldEqual StatusCodes.NotFound
            case Some(h) =>
              status shouldEqual StatusCodes.OK
              responseAs[JsValue] shouldEqual (tx.json + ("height" -> JsNumber(h)))
          }
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
}
