package com.wavesplatform.it.api

import com.google.common.primitives.Longs
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.crypto
import com.wavesplatform.http.api_key
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.AsyncHttpApi.NodeAsyncHttpApi
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.utils.Base58
import org.asynchttpclient.Dsl.{get => _get}
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{RequestBuilder, Response}
import org.scalatest.Assertions
import play.api.libs.json.Json.{parse, stringify, toJson}
import play.api.libs.json.{Json, Writes}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object AsyncMatcherHttpApi extends Assertions {

  val DefaultMatcherFee: Int = 300000

  implicit class MatcherAsyncHttpApi(matcherNode: Node) extends NodeAsyncHttpApi(matcherNode) {

    def matcherGet(path: String,
                   f: RequestBuilder => RequestBuilder = identity,
                   statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                   waitForStatus: Boolean = false): Future[Response] =
      retrying(f(_get(s"${matcherNode.matcherApiEndpoint}$path")).build(), statusCode = statusCode, waitForStatus = waitForStatus)

    def matcherGetWithApiKey(path: String,
                             f: RequestBuilder => RequestBuilder = identity,
                             statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                             waitForStatus: Boolean = false): Future[Response] = retrying(
      _get(s"${matcherNode.matcherApiEndpoint}$path")
        .withApiKey(matcherNode.apiKey)
        .build()
    )

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

    def postWithAPiKey(path: String): Future[Response] =
      post(
        s"${matcherNode.matcherApiEndpoint}$path",
        (rb: RequestBuilder) =>
          rb.withApiKey(matcherNode.apiKey)
            .setHeader("Content-type", "application/json;charset=utf-8")
      )

    def orderStatus(orderId: String, assetPair: AssetPair, waitForStatus: Boolean = true): Future[MatcherStatusResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset/$orderId", waitForStatus = waitForStatus)
        .as[MatcherStatusResponse]
    }

    def transactionsByOrder(orderId: String): Future[Seq[ExchangeTransaction]] =
      matcherGet(s"/matcher/transactions/$orderId").as[Seq[ExchangeTransaction]]

    def orderBook(assetPair: AssetPair): Future[OrderBookResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset").as[OrderBookResponse]
    }

    def marketStatus(assetPair: AssetPair): Future[MarketStatusResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset/status").as[MarketStatusResponse]
    }

    def parseAssetPair(assetPair: AssetPair): (String, String) = {
      val amountAsset = assetPair.amountAsset match {
        case None => "WAVES"
        case _    => assetPair.amountAsset.get.base58
      }
      val priceAsset = assetPair.priceAsset match {
        case None => "WAVES"
        case _    => assetPair.priceAsset.get.base58
      }
      (amountAsset, priceAsset)
    }

    def cancelOrder(sender: PrivateKeyAccount,
                    assetPair: AssetPair,
                    orderId: Option[String],
                    timestamp: Option[Long]): Future[MatcherStatusResponse] = {
      val privateKey                = sender.privateKey
      val publicKey                 = PublicKeyAccount(sender.publicKey)
      val request                   = CancelOrderRequest(publicKey, orderId.map(ByteStr.decodeBase58(_).get), timestamp, Array.emptyByteArray)
      val sig                       = crypto.sign(privateKey, request.toSign)
      val signedRequest             = request.copy(signature = sig)
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherPost(s"/matcher/orderbook/$amountAsset/$priceAsset/cancel", signedRequest).as[MatcherStatusResponse]
    }

    def cancelAllOrders(sender: PrivateKeyAccount, timestamp: Option[Long]): Future[MatcherStatusResponse] = {
      val privateKey    = sender.privateKey
      val request       = CancelOrderRequest(PublicKeyAccount(sender.publicKey), None, timestamp, Array.emptyByteArray)
      val sig           = crypto.sign(privateKey, request.toSign)
      val signedRequest = request.copy(signature = sig)
      matcherPost(s"/matcher/orderbook/cancel", Json.toJson(signedRequest)).as[MatcherStatusResponse]
    }

    def cancelOrderWithApiKey(orderId: String): Future[MatcherStatusResponse] = {
      postWithAPiKey(s"/matcher/orders/cancel/$orderId").as[MatcherStatusResponse]
    }

    def fullOrdersHistory(sender: PrivateKeyAccount): Future[Seq[OrderbookHistory]] = {
      val ts = System.currentTimeMillis()
      matcherGetWithSignature(s"/matcher/orderbook/${Base58.encode(sender.publicKey)}", ts, signature(sender, ts)).as[Seq[OrderbookHistory]]
    }

    def orderHistoryByPair(sender: PrivateKeyAccount, assetPair: AssetPair): Future[Seq[OrderbookHistory]] = {
      val ts                        = System.currentTimeMillis()
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGetWithSignature(s"/matcher/orderbook/$amountAsset/$priceAsset/publicKey/${Base58.encode(sender.publicKey)}", ts, signature(sender, ts))
        .as[Seq[OrderbookHistory]]
    }

    def activeOrderHistory(sender: PrivateKeyAccount): Future[Seq[OrderbookHistory]] = {
      val ts = System.currentTimeMillis()
      matcherGetWithSignature(s"/matcher/orderbook/${Base58.encode(sender.publicKey)}?activeOnly=true", ts, signature(sender, ts))
        .as[Seq[OrderbookHistory]]
    }

    def reservedBalance(sender: PrivateKeyAccount): Future[Map[String, Long]] = {
      val ts         = System.currentTimeMillis()
      val privateKey = sender.privateKey
      val publicKey  = sender.publicKey
      val signature  = ByteStr(crypto.sign(privateKey, publicKey ++ Longs.toByteArray(ts)))

      matcherGetWithSignature(s"/matcher/balance/reserved/${Base58.encode(sender.publicKey)}", ts, signature).as[Map[String, Long]]
    }

    def tradableBalance(sender: PrivateKeyAccount, assetPair: AssetPair): Future[Map[String, Long]] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset/tradableBalance/${sender.address}").as[Map[String, Long]]
    }

    def tradingMarkets(): Future[MarketDataInfo] =
      matcherGet(s"/matcher/orderbook").as[MarketDataInfo]

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

    def waitOrderStatusAndAmount(assetPair: AssetPair,
                                 orderId: String,
                                 expectedStatus: String,
                                 expectedFilledAmount: Option[Long],
                                 retryInterval: FiniteDuration = 1.second): Future[MatcherStatusResponse] = {
      waitFor[MatcherStatusResponse](
        s"order(amountAsset=${assetPair.amountAsset}, priceAsset=${assetPair.priceAsset}, orderId=$orderId) status == $expectedStatus")(
        _.orderStatus(orderId, assetPair),
        s => s.status == expectedStatus && s.filledAmount == expectedFilledAmount,
        5.seconds)
    }

    def prepareOrder(sender: PrivateKeyAccount,
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
        Order(sender, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLiveTimestamp, 300000, Proofs.empty, version)
      Order.sign(unsigned, sender)
    }

    def placeOrder(order: Order): Future[MatcherResponse] =
      matcherPost("/matcher/orderbook", order.json()).as[MatcherResponse]

    def placeOrder(sender: PrivateKeyAccount,
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

    def ordersByAddress(sender: PrivateKeyAccount, activeOnly: Boolean): Future[Seq[OrderbookHistory]] =
      ordersByAddress(sender.address, activeOnly)

    def ordersByAddress(senderAddress: String, activeOnly: Boolean): Future[Seq[OrderbookHistory]] =
      matcherGetWithApiKey(s"/matcher/orders/$senderAddress?activeOnly=$activeOnly").as[Seq[OrderbookHistory]]

    private def signature(node: PrivateKeyAccount, timestamp: Long) = {
      val privateKey = node.privateKey
      val publicKey  = node.publicKey
      ByteStr(crypto.sign(privateKey, publicKey ++ Longs.toByteArray(timestamp)))
    }

  }

  implicit class RequestBuilderOps(self: RequestBuilder) {
    def withApiKey(x: String): RequestBuilder = self.setHeader(api_key.name, x)
  }

}
