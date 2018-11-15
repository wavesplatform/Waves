package com.wavesplatform.it.api

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.account.{PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.it.Node
import com.wavesplatform.it.api.SyncHttpApi.RequestAwaitTime
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{RequestBuilder, Response}
import org.scalatest.{Assertion, Assertions, Matchers}
import play.api.libs.json.Json.parse
import play.api.libs.json.{Format, Json, Writes}

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable}
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

object SyncMatcherHttpApi extends Assertions {

  case class NotFoundErrorMessage(message: String)
  case class ErrorMessage(error: Int, message: String)

  object NotFoundErrorMessage {
    implicit val format: Format[NotFoundErrorMessage] = Json.format
  }

  def assertNotFoundAndMessage[R](f: => R, errorMessage: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
      Assertions.assert(statusCode == StatusCodes.NotFound.intValue && parse(responseBody).as[NotFoundErrorMessage].message.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail(s"Expecting not found error")
  }

  def sync[A](awaitable: Awaitable[A], atMost: Duration = RequestAwaitTime) =
    try Await.result(awaitable, atMost)
    catch {
      case usce: UnexpectedStatusCodeException => throw usce
      case NonFatal(cause) =>
        throw new Exception(cause)
    }

  implicit class MatcherNodeExtSync(m: Node) extends Matchers {

    import com.wavesplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

    private val RequestAwaitTime      = 30.seconds
    private val OrderRequestAwaitTime = 1.minutes

    def orderBook(assetPair: AssetPair): OrderBookResponse =
      sync(async(m).orderBook(assetPair))

    def orderBookExpectInvalidAssetId(assetPair: AssetPair, assetId: String): Boolean =
      Await.result(async(m).orderBookExpectInvalidAssetId(assetPair, assetId), OrderRequestAwaitTime)

    def marketStatus(assetPair: AssetPair): MarketStatusResponse =
      sync(async(m).marketStatus(assetPair), RequestAwaitTime)

    def deleteOrderBook(assetPair: AssetPair): OrderBookResponse =
      sync(async(m).deleteOrderBook(assetPair), RequestAwaitTime)

    def fullOrderHistory(sender: PrivateKeyAccount): Seq[OrderbookHistory] =
      sync(async(m).fullOrdersHistory(sender), RequestAwaitTime)

    def orderHistoryByPair(sender: PrivateKeyAccount, assetPair: AssetPair, activeOnly: Boolean = false): Seq[OrderbookHistory] =
      sync(async(m).orderHistoryByPair(sender, assetPair, activeOnly), RequestAwaitTime)

    def activeOrderHistory(sender: PrivateKeyAccount): Seq[OrderbookHistory] =
      sync(async(m).activeOrderHistory(sender))

    def placeOrder(order: Order): MatcherResponse =
      sync(async(m).placeOrder(order))

    def placeOrder(sender: PrivateKeyAccount,
                   pair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   fee: Long,
                   version: Byte = 1: Byte,
                   timeToLive: Duration = 30.days - 1.seconds): MatcherResponse =
      sync(async(m).placeOrder(sender, pair, orderType, amount, price, fee, version, timeToLive))

    def orderStatus(orderId: String, assetPair: AssetPair, waitForStatus: Boolean = true): MatcherStatusResponse =
      sync(async(m).orderStatus(orderId, assetPair, waitForStatus))

    def orderStatusExpectInvalidAssetId(orderId: String, assetPair: AssetPair, assetId: String): Boolean =
      Await.result(async(m).orderStatusExpectInvalidAssetId(orderId, assetPair, assetId), OrderRequestAwaitTime)

    def transactionsByOrder(orderId: String): Seq[ExchangeTransaction] =
      sync(async(m).transactionsByOrder(orderId))

    def waitOrderStatus(assetPair: AssetPair,
                        orderId: String,
                        expectedStatus: String,
                        waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).waitOrderStatus(assetPair, orderId, expectedStatus), waitTime)

    def waitOrderStatusAndAmount(assetPair: AssetPair,
                                 orderId: String,
                                 expectedStatus: String,
                                 expectedFilledAmount: Option[Long],
                                 waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).waitOrderStatusAndAmount(assetPair, orderId, expectedStatus, expectedFilledAmount), waitTime)

    def waitOrderInBlockchain(orderId: String,
                              retryInterval: FiniteDuration = 1.second,
                              waitTime: Duration = OrderRequestAwaitTime): Seq[TransactionInfo] =
      sync(async(m).waitOrderInBlockchain(orderId, retryInterval), waitTime)

    def reservedBalance(sender: PrivateKeyAccount, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      sync(async(m).reservedBalance(sender), waitTime)

    def tradableBalance(sender: PrivateKeyAccount, assetPair: AssetPair, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      sync(async(m).tradableBalance(sender, assetPair), waitTime)

    def tradingMarkets(waitTime: Duration = OrderRequestAwaitTime): MarketDataInfo =
      sync(async(m).tradingMarkets(), waitTime)

    def expectIncorrectOrderPlacement(order: Order,
                                      expectedStatusCode: Int,
                                      expectedStatus: String,
                                      expectedMessage: Option[String] = None,
                                      waitTime: Duration = OrderRequestAwaitTime): Boolean =
      sync(async(m).expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus, expectedMessage), waitTime)

    def expectRejectedOrderPlacement(sender: PrivateKeyAccount,
                                     pair: AssetPair,
                                     orderType: OrderType,
                                     amount: Long,
                                     price: Long,
                                     fee: Long = 300000L,
                                     version: Byte = 1,
                                     timeToLive: Duration = 30.days - 1.seconds,
                                     expectedMessage: Option[String] = None): Boolean =
      expectIncorrectOrderPlacement(prepareOrder(sender, pair, orderType, amount, price, fee, version, timeToLive),
                                    400,
                                    "OrderRejected",
                                    expectedMessage)

    def expectCancelRejected(sender: PrivateKeyAccount, assetPair: AssetPair, orderId: String, waitTime: Duration = OrderRequestAwaitTime): Unit =
      sync(async(m).expectCancelRejected(sender, assetPair, orderId), waitTime)

    def cancelOrder(sender: PrivateKeyAccount,
                    assetPair: AssetPair,
                    orderId: String,
                    waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelOrder(sender, assetPair, orderId), waitTime)

    def cancelOrdersForPair(sender: PrivateKeyAccount,
                            assetPair: AssetPair,
                            timestamp: Long = System.currentTimeMillis(),
                            waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelOrdersForPair(sender, assetPair, timestamp), waitTime)

    def cancelAllOrders(sender: PrivateKeyAccount,
                        timestamp: Long = System.currentTimeMillis(),
                        waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelAllOrders(sender, timestamp), waitTime)

    def cancelOrderWithApiKey(orderId: String, waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelOrderWithApiKey(orderId), waitTime)

    def matcherGet(path: String,
                   f: RequestBuilder => RequestBuilder = identity,
                   statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                   waitForStatus: Boolean = false,
                   waitTime: Duration = RequestAwaitTime): Response =
      sync(async(m).matcherGet(path, f, statusCode, waitForStatus), waitTime)

    def matcherGetStatusCode(path: String, statusCode: Int, waitTime: Duration = RequestAwaitTime): MessageMatcherResponse =
      sync(async(m).matcherGetStatusCode(path, statusCode), waitTime)

    def matcherPost[A: Writes](path: String, body: A, waitTime: Duration = RequestAwaitTime): Response =
      sync(async(m).matcherPost(path, body), waitTime)

    def prepareOrder(sender: PrivateKeyAccount,
                     pair: AssetPair,
                     orderType: OrderType,
                     amount: Long,
                     price: Long,
                     fee: Long = 300000L,
                     version: Byte = 1: Byte,
                     timeToLive: Duration = 30.days - 1.seconds): Order = {
      val creationTime        = System.currentTimeMillis()
      val timeToLiveTimestamp = creationTime + timeToLive.toMillis
      val matcherPublicKey    = m.publicKey
      val unsigned =
        Order(PublicKeyAccount(sender.publicKey),
              matcherPublicKey,
              pair,
              orderType,
              amount,
              price,
              creationTime,
              timeToLiveTimestamp,
              fee,
              Proofs.empty,
              version)
      Order.sign(unsigned, sender)
    }

    def ordersByAddress(sender: PrivateKeyAccount, activeOnly: Boolean, waitTime: Duration = RequestAwaitTime): Seq[OrderbookHistory] =
      sync(async(m).ordersByAddress(sender, activeOnly), waitTime)
  }

}
