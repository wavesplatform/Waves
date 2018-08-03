package com.wavesplatform.it.api

import com.wavesplatform.crypto
import com.wavesplatform.it.Node
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{RequestBuilder, Response}
import org.scalatest.{Assertions, Matchers}
import play.api.libs.json.{Format, Json, Writes}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.Await
import scala.concurrent.duration._

object SyncMatcherHttpApi extends Assertions {
  case class ErrorMessage(error: Int, message: String)

  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

  implicit class MatcherNodeExtSync(m: Node) extends Matchers {

    import com.wavesplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

    private val RequestAwaitTime      = 15.seconds
    private val OrderRequestAwaitTime = 1.minutes

    def orderBook(assetPair: AssetPair): OrderBookResponse =
      Await.result(async(m).orderBook(assetPair), RequestAwaitTime)

    def orderHistory(sender: Node): Seq[OrderbookHistory] =
      Await.result(async(m).orderHistory(sender), RequestAwaitTime)

    def activeOrderHistory(sender: Node): Seq[OrderbookHistory] =
      Await.result(async(m).activeOrderHistory(sender), RequestAwaitTime)

    def placeOrder(order: Order): MatcherResponse =
      Await.result(async(m).placeOrder(order), RequestAwaitTime)

    def placeOrder(sender: Node,
                   pair: AssetPair,
                   orderType: OrderType,
                   price: Long,
                   amount: Long,
                   timeToLive: Duration = 30.days - 1.seconds): MatcherResponse =
      Await.result(async(m).placeOrder(sender, pair, orderType, price, amount, timeToLive), RequestAwaitTime)

    def orderStatus(orderId: String, assetPair: AssetPair, waitForStatus: Boolean = true): MatcherStatusResponse =
      Await.result(async(m).orderStatus(orderId, assetPair, waitForStatus), RequestAwaitTime)

    def transactionsByOrder(orderId: String): Seq[Transaction] =
      Await.result(async(m).transactionsByOrder(orderId), RequestAwaitTime)

    def waitOrderStatus(assetPair: AssetPair,
                        orderId: String,
                        expectedStatus: String,
                        waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(m).waitOrderStatus(assetPair, orderId, expectedStatus), waitTime)

    def reservedBalance(sender: Node, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      Await.result(async(m).reservedBalance(sender), waitTime)

    def tradableBalance(sender: Node, assetPair: AssetPair, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      Await.result(async(m).tradableBalance(sender, assetPair), waitTime)

    def expectIncorrectOrderPlacement(order: Order,
                                      expectedStatusCode: Int,
                                      expectedStatus: String,
                                      waitTime: Duration = OrderRequestAwaitTime): Boolean =
      Await.result(async(m).expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus), waitTime)

    def cancelOrder(sender: Node,
                    assetPair: AssetPair,
                    orderId: String,
                    timestamp: Option[Long] = None,
                    waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(m).cancelOrder(sender, assetPair, orderId, timestamp), waitTime)

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
                     price: Long,
                     amount: Long,
                     timeToLive: Duration = 30.days - 1.seconds): Order = {
      val creationTime        = System.currentTimeMillis()
      val timeToLiveTimestamp = creationTime + timeToLive.toMillis
      val matcherPublicKey    = m.publicKey
      val unsigned            = Order(node.publicKey, matcherPublicKey, pair, orderType, price, amount, creationTime, timeToLiveTimestamp, 300000, Array())
      val signature           = crypto.sign(node.privateKey, unsigned.toSign)
      unsigned.copy(signature = signature)
    }
  }

}
