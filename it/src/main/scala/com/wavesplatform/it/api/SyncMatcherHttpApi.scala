package com.wavesplatform.it.api

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.crypto
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.it.Node
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{RequestBuilder, Response}
import org.scalatest.{Assertion, Assertions, Matchers}
import play.api.libs.json.Json.parse
import play.api.libs.json.{Format, Json, Writes}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Try}

object SyncMatcherHttpApi extends Assertions {
  case class ErrorMessage(error: Int, message: String)

  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

  case class NotFoundErrorMessage(message: String)

  object NotFoundErrorMessage {
    implicit val format: Format[NotFoundErrorMessage] = Json.format
  }

  def assertNotFoundAndMessage[R](f: => R, errorMessage: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      Assertions.assert(statusCode == StatusCodes.NotFound.intValue && parse(responseBody).as[NotFoundErrorMessage].message.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail(s"Expecting not found error")
  }

  implicit class MatcherNodeExtSync(m: Node) {

    import com.wavesplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

    private val RequestAwaitTime      = 30.seconds
    private val OrderRequestAwaitTime = 1.minutes

    def orderBook(assetPair: AssetPair): OrderBookResponse =
      Await.result(async(m).orderBook(assetPair), RequestAwaitTime)

    def marketStatus(assetPair: AssetPair): MarketStatusResponse =
      Await.result(async(m).marketStatus(assetPair), RequestAwaitTime)

    def deleteOrderBook(assetPair: AssetPair): OrderBookResponse =
      Await.result(async(m).deleteOrderBook(assetPair), RequestAwaitTime)

    def fullOrderHistory(sender: Node): Seq[OrderbookHistory] =
      Await.result(async(m).fullOrdersHistory(sender), RequestAwaitTime)

    def orderHistoryByPair(sender: Node, assetPair: AssetPair, activeOnly: Boolean = false): Seq[OrderbookHistory] =
      Await.result(async(m).orderHistoryByPair(sender, assetPair, activeOnly), RequestAwaitTime)

    def activeOrderHistory(sender: Node): Seq[OrderbookHistory] =
      Await.result(async(m).activeOrderHistory(sender), RequestAwaitTime)

    def placeOrder(order: Order): MatcherResponse =
      Await.result(async(m).placeOrder(order), RequestAwaitTime)

    def placeOrder(sender: Node,
                   pair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   timeToLive: Duration = 30.days - 1.seconds): MatcherResponse =
      Await.result(async(m).placeOrder(sender, pair, orderType, amount, price, timeToLive), RequestAwaitTime)

    def orderStatus(orderId: String, assetPair: AssetPair, waitForStatus: Boolean = true): MatcherStatusResponse =
      Await.result(async(m).orderStatus(orderId, assetPair, waitForStatus), RequestAwaitTime)

    def transactionsByOrder(orderId: String): Seq[ExchangeTransaction] =
      Await.result(async(m).transactionsByOrder(orderId), RequestAwaitTime)

    def waitOrderStatus(assetPair: AssetPair,
                        orderId: String,
                        expectedStatus: String,
                        waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(m).waitOrderStatus(assetPair, orderId, expectedStatus), waitTime)

    def waitOrderStatusAndAmount(assetPair: AssetPair,
                                 orderId: String,
                                 expectedStatus: String,
                                 expectedFilledAmount: Option[Long],
                                 waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(m).waitOrderStatusAndAmount(assetPair, orderId, expectedStatus, expectedFilledAmount), waitTime)

    def waitOrderInBlockchain(orderId: String, retryInterval: FiniteDuration = 1.second): Unit =
      Await.result(async(m).waitOrderInBlockchain(orderId, retryInterval), RequestAwaitTime)

    def reservedBalance(sender: Node, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      reservedBalance(sender.privateKey, waitTime)

    def reservedBalance(sender: PrivateKeyAccount, waitTime: Duration): Map[String, Long] =
      Await.result(async(m).reservedBalance(sender), waitTime)

    def tradableBalance(sender: Node, assetPair: AssetPair, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      tradableBalance(sender.privateKey, assetPair, waitTime)

    def tradableBalance(sender: PrivateKeyAccount, assetPair: AssetPair, waitTime: Duration): Map[String, Long] =
      Await.result(async(m).tradableBalance(sender, assetPair), waitTime)

    def tradingMarkets(waitTime: Duration = OrderRequestAwaitTime): MarketDataInfo =
      Await.result(async(m).tradingMarkets(), waitTime)

    def expectIncorrectOrderPlacement(order: Order,
                                      expectedStatusCode: Int,
                                      expectedStatus: String,
                                      waitTime: Duration = OrderRequestAwaitTime): Boolean =
      Await.result(async(m).expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus), waitTime)

    def expectCancelRejected(sender: PrivateKeyAccount, assetPair: AssetPair, orderId: String, waitTime: Duration = OrderRequestAwaitTime): Unit =
      Await.result(async(m).expectCancelRejected(sender, assetPair, orderId), waitTime)

    def cancelOrder(sender: Node, assetPair: AssetPair, orderId: String, waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      cancelOrder(sender.privateKey, assetPair, orderId, waitTime)

    def cancelOrder(sender: PrivateKeyAccount, assetPair: AssetPair, orderId: String, waitTime: Duration): MatcherStatusResponse =
      Await.result(async(m).cancelOrder(sender, assetPair, orderId), waitTime)

    def cancelOrdersForPair(sender: Node,
                            assetPair: AssetPair,
                            timestamp: Long = System.currentTimeMillis(),
                            waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(m).cancelOrdersForPair(sender, assetPair, timestamp), waitTime)

    def cancelAllOrders(sender: Node,
                        timestamp: Long = System.currentTimeMillis(),
                        waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(m).cancelAllOrders(sender, timestamp), waitTime)

    def cancelOrderWithApiKey(orderId: String, waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(m).cancelOrderWithApiKey(orderId), waitTime)

    def matcherGet(path: String,
                   f: RequestBuilder => RequestBuilder = identity,
                   statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                   waitForStatus: Boolean = false,
                   waitTime: Duration = RequestAwaitTime): Response =
      Await.result(async(m).matcherGet(path, f, statusCode, waitForStatus), waitTime)

    def matcherGetStatusCode(path: String, statusCode: Int, waitTime: Duration = RequestAwaitTime): MessageMatcherResponse =
      Await.result(async(m).matcherGetStatusCode(path, statusCode), waitTime)

    def matcherPost[A: Writes](path: String, body: A, waitTime: Duration = RequestAwaitTime): Response =
      Await.result(async(m).matcherPost(path, body), waitTime)

    def prepareOrder(node: Node,
                     pair: AssetPair,
                     orderType: OrderType,
                     amount: Long,
                     price: Long,
                     timeToLive: Duration = 30.days - 1.seconds): Order = {
      val creationTime        = System.currentTimeMillis()
      val timeToLiveTimestamp = creationTime + timeToLive.toMillis
      val matcherPublicKey    = m.publicKey
      val unsigned = Order(node.publicKey,
                           matcherPublicKey,
                           pair,
                           orderType,
                           amount,
                           price,
                           creationTime,
                           timeToLiveTimestamp,
                           AsyncMatcherHttpApi.DefaultMatcherFee,
                           Array())
      val signature = crypto.sign(node.privateKey, unsigned.toSign)
      unsigned.copy(signature = signature)
    }

    def ordersByAddress(sender: Node, activeOnly: Boolean, waitTime: Duration = RequestAwaitTime): Seq[OrderbookHistory] =
      ordersByAddress(sender.address, activeOnly, waitTime)

    def ordersByAddress(senderAddress: String, activeOnly: Boolean, waitTime: Duration): Seq[OrderbookHistory] =
      Await.result(async(m).ordersByAddress(senderAddress, activeOnly), waitTime)
  }

}
