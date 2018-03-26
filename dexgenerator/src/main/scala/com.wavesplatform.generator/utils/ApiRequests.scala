package com.wavesplatform.generator.utils

import java.io.IOException
import java.util.concurrent.TimeoutException

import com.wavesplatform.it.api.{AssetBalance, Balance, MatcherResponse, ResponseFutureExt, Transaction, UnexpectedStatusCodeException}
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import play.api.libs.json.Json.{parse, stringify, toJson}
import play.api.libs.json._
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.scalatest.{Assertions, Matchers}
import play.api.libs.json.Json.{parse, stringify, toJson}
import play.api.libs.json._
import scorex.api.http.assets.{SignedIssueRequest, SignedMassTransferRequest}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.assets.exchange.Order
import scorex.utils.ScorexLogging

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ApiRequests(client: AsyncHttpClient) extends ScorexLogging {

  def retrying(r: Request, interval: FiniteDuration = 1.second, statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.trace(s"Executing request '$r'")
      client.executeRequest(r, new AsyncCompletionHandler[Response] {
        override def onCompleted(response: Response): Response = {
          if (response.getStatusCode == statusCode) {
            log.debug(s"Request: ${r.getUrl}\nResponse: ${response.getResponseBody}")
            response
          } else {
            log.debug(s"Request: ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
            throw UnexpectedStatusCodeException(r.getUrl, response.getStatusCode, response.getResponseBody)
          }
        }
      }).toCompletableFuture.toScala
        .recoverWith {
          case e@(_: IOException | _: TimeoutException) =>
            log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }


  def createSignedIssueRequest (tx: IssueTransaction): SignedIssueRequest = {

    import tx._

    SignedIssueRequest (
      Base58.encode (tx.sender.publicKey),
      new String (name),
      new String (description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }

  def to(endpoint: String) = new Node(endpoint)

  class Node(endpoint: String) {


    def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_get(s"$endpoint$path")).build())

    def post(url: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_post(url)).build())

    def post(path: String, body: String): Future[Response] =
      post(s"$endpoint$path",
        (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

    def postJson[A: Writes](path: String, body: A): Future[Response] =
      post(path, stringify(toJson(body)))

    def matcherPost[A: Writes](path: String, body: A): Future[Response] =
      post(s"$endpoint$path",
        (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

    def placeOrder(order: Order): Future[Response] =
      matcherPost("/matcher/orderbook", order.json())

    def matcherPost[A: Writes](path: String, body: A): Future[Response] =
      post(s"$endpoint$path",
        (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

    def height(endpoint: String): Future[Int] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Int])

    def transactionInfo(txId: String): Future[Transaction] = get(s"/transactions/info/$txId").as[Transaction]

    def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

    def assetBalance(address: String, asset: String): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset").as[AssetBalance]

    def signedIssue(issue: SignedIssueRequest): Future[Transaction] =
      postJson("/assets/broadcast/issue", issue).as[Transaction]

    def signedMassTransfer(massTx: SignedMassTransferRequest): Future[Transaction] =
      postJson("/assets/broadcast/issue", massTx).as[Transaction]

    def broadcastRequest[A: Writes](req: A): Future[Transaction] = postJson("/transactions/broadcast", req).as[Transaction]


    def waitForTransaction (txId: String, retryInterval: FiniteDuration = 1.second): Future[Transaction] =
      waitFor[Option[Transaction]] (s"transaction $txId") (_.transactionInfo (txId).transform {
        case Success (tx) => Success (Some (tx) )
        case Failure (UnexpectedStatusCodeException (_, 404, _) ) => Success (None)
        case Failure (ex) => Failure (ex)
      }, tOpt => tOpt.exists (_.id == txId), retryInterval).map (_.get)

    private val RequestAwaitTime = 15.seconds


    def waitFor[A](desc: String)(f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] = {
      log.debug(s"Awaiting condition '$desc'")
      timer.retryUntil(f(this), cond, retryInterval)
        .map(a => {
          log.debug(s"Condition '$desc' met")
          a
        })
    }
  }


}
