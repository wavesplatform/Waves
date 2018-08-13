package com.wavesplatform.it.api

import com.google.common.primitives.Longs
import com.wavesplatform.crypto
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.AsyncHttpApi.NodeAsyncHttpApi
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state.ByteStr
import org.asynchttpclient.Dsl.{get => _get}
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{RequestBuilder, Response}
import org.scalatest.Assertions
import play.api.libs.json.Json.{parse, stringify, toJson}
import play.api.libs.json.Writes
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.Proofs

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.{Failure, Success, Try}

object AsyncMatcherHttpApi extends Assertions {

  implicit class MatcherAsyncHttpApi(matcherNode: Node) extends NodeAsyncHttpApi(matcherNode) {

    def matcherGet(path: String,
                   f: RequestBuilder => RequestBuilder = identity,
                   statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                   waitForStatus: Boolean = false): Future[Response] =
      retrying(f(_get(s"${matcherNode.matcherApiEndpoint}$path")).build(), statusCode = statusCode, waitForStatus = waitForStatus)

    def matcherGetWithSignature(path: String, ts: Long, signature: ByteStr, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying {
        _get(s"${matcherNode.matcherApiEndpoint}$path")
          .setHeader("Timestamp", ts)
          .setHeader("Signature", signature)
          .build()
      }

    def matcherGetStatusCode(path: String, statusCode: Int): Future[MessageMatcherResponse] =
      matcherGet(path, statusCode = statusCode).as[MessageMatcherResponse]

    def matcherPost[A: Writes](path: String, body: A): Future[Response] =
      post(s"${matcherNode.matcherApiEndpoint}$path",
           (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

    def orderStatus(orderId: String, assetPair: AssetPair, waitForStatus: Boolean = true): Future[MatcherStatusResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/${amountAsset}/${priceAsset}/$orderId", waitForStatus = waitForStatus)
        .as[MatcherStatusResponse]
    }

    def orderBook(assetPair: AssetPair): Future[OrderBookResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/${amountAsset}/${priceAsset}").as[OrderBookResponse]
    }

    def marketStatus(assetPair: AssetPair): Future[MarketStatusResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset/status").as[MarketStatusResponse]
    }

    def parseAssetPair(assetPair: AssetPair) = {
      val amountAsset = assetPair.amountAsset match {
        case None => "WAVES"
        case _    => assetPair.amountAsset.get
      }
      val priceAsset = assetPair.priceAsset match {
        case None => "WAVES"
        case _    => assetPair.priceAsset.get
      }
      (amountAsset, priceAsset)
    }

    def cancelOrder(sender: Node, assetPair: AssetPair, orderId: Option[String], timestamp: Option[Long] = None) = {
      val privateKey                = sender.privateKey
      val publicKey                 = sender.publicKey
      val request                   = CancelOrderRequest(publicKey, orderId, timestamp, Array.emptyByteArray)
      val sig                       = crypto.sign(privateKey, request.toSign)
      val signedRequest             = request.copy(signature = sig)
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherPost(s"/matcher/orderbook/${amountAsset}/${priceAsset}/cancel", signedRequest.json).as[MatcherStatusResponse]
    }

    def cancelAllOrders(sender: Node, timestamp: Option[Long]) = {
      val privateKey    = sender.privateKey
      val publicKey     = sender.publicKey
      val request       = CancelOrderRequest(publicKey, None, timestamp, Array.emptyByteArray)
      val sig           = crypto.sign(privateKey, request.toSign)
      val signedRequest = request.copy(signature = sig)
      matcherPost(s"/matcher/orderbook/cancel", signedRequest.json).as[MatcherStatusResponse]
    }

    def orderHistory(sender: Node) = {
      val ts = System.currentTimeMillis()
      matcherGetWithSignature(s"/matcher/orderbook/${sender.publicKeyStr}", ts, signature(sender, ts)).as[Seq[OrderbookHistory]]
    }

    def activeOrderHistory(sender: Node) = {
      val ts = System.currentTimeMillis()
      matcherGetWithSignature(s"/matcher/orderbook/${sender.publicKeyStr}?activeOnly=true", ts, signature(sender, ts)).as[Seq[OrderbookHistory]]
    }

    def reservedBalance(sender: Node) = {
      val ts         = System.currentTimeMillis()
      val privateKey = sender.privateKey
      val publicKey  = sender.publicKey.publicKey
      val signature  = ByteStr(crypto.sign(privateKey, publicKey ++ Longs.toByteArray(ts)))

      matcherGetWithSignature(s"/matcher/balance/reserved/${sender.publicKeyStr}", ts, signature).as[Map[String, Long]]
    }

    def waitOrderStatus(assetPair: AssetPair,
                        orderId: String,
                        expectedStatus: String,
                        retryInterval: FiniteDuration = 1.second): Future[MatcherStatusResponse] = {
      waitFor[MatcherStatusResponse](
        s"order(amountAsset=${assetPair.amountAsset}, priceAsset=${assetPair.priceAsset}, orderId=$orderId) status == $expectedStatus")(
        _.orderStatus(orderId, assetPair),
        _.status == expectedStatus,
        5.seconds)
    }

    def prepareOrder(sender: Node,
                     pair: AssetPair,
                     orderType: OrderType,
                     price: Long,
                     amount: Long,
                     version: Byte,
                     timeToLive: Duration = 30.days - 1.seconds,
    ): Order = {
      val creationTime        = System.currentTimeMillis()
      val timeToLiveTimestamp = creationTime + timeToLive.toMillis
      val matcherPublicKey    = matcherNode.publicKey
      val unsigned =
        Order(sender.publicKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLiveTimestamp, 300000, Proofs.empty, version)
      val signature = crypto.sign(sender.privateKey, unsigned.bodyBytes())
      unsigned.updateProofs(Proofs(Seq(ByteStr(signature))))
    }

    def placeOrder(order: Order): Future[MatcherResponse] =
      matcherPost("/matcher/orderbook", order.json()).as[MatcherResponse]

    def placeOrder(sender: Node,
                   pair: AssetPair,
                   orderType: OrderType,
                   price: Long,
                   amount: Long,
                   version: Byte,
                   timeToLive: Duration = 30.days - 1.seconds): Future[MatcherResponse] = {
      val order = prepareOrder(sender, pair, orderType, price, amount, version, timeToLive)
      matcherPost("/matcher/orderbook", order.json()).as[MatcherResponse]
    }

    def expectIncorrectOrderPlacement(order: Order, expectedStatusCode: Int, expectedStatus: String): Future[Boolean] =
      matcherPost("/matcher/orderbook", order.json()) transform {
        case Failure(UnexpectedStatusCodeException(_, `expectedStatusCode`, responseBody)) =>
          Try(parse(responseBody).as[MatcherStatusResponse]) match {
            case Success(mr) if mr.status == expectedStatus => Success(true)
            case Failure(f)                                 => Failure(new RuntimeException(s"Failed to parse response: $f"))
          }
        case Success(r) => Failure(new RuntimeException(s"Unexpected matcher response: (${r.getStatusCode}) ${r.getResponseBody}"))
        case _          => Failure(new RuntimeException(s"Unexpected failure from matcher"))
      }

    private def signature(node: Node, timestamp: Long) = {
      val privateKey = node.privateKey
      val publicKey  = node.publicKey.publicKey
      ByteStr(crypto.sign(privateKey, publicKey ++ Longs.toByteArray(timestamp)))
    }

  }

}
