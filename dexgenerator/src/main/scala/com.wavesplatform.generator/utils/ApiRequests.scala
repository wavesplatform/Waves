package com.wavesplatform.generator.utils

import java.io.IOException

import java.util.concurrent.TimeoutException

import com.wavesplatform.it.api.{MatcherResponse, UnexpectedStatusCodeException}
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import play.api.libs.json.Json.{parse, stringify, toJson}
import play.api.libs.json._
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import play.api.libs.json.Json.{parse, stringify, toJson}
import play.api.libs.json._
import scorex.transaction.assets.exchange.Order
import scorex.utils.ScorexLogging

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

class ApiRequests(client: AsyncHttpClient) extends ScorexLogging{

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

  def post(url: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_post(url)).build())

  def post(endpoint: String, path: String, body: String): Future[Response] =
    post(s"$endpoint$path",
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

  def postJson[A: Writes](endpoint: String, path: String, body: A): Future[Response] =
    post(endpoint, path, stringify(toJson(body)))

  def matcherPost[A: Writes](endpoint: String, path: String, body: A): Future[Response] =
    post(s"$endpoint$path",
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

  def placeOrder(endpoint: String,order: Order): Future[Response] =
    matcherPost(endpoint, "/matcher/orderbook", order.json())

  def matcherPost[A: Writes](endpoint: String, path: String, body: A): Future[Response] =
    post(s"$endpoint$path",
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))


}
