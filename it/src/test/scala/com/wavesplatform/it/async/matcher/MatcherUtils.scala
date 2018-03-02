package com.wavesplatform.it.matcher

import com.wavesplatform.it.Node
import com.wavesplatform.it.api.{AssetBalance, MatcherStatusResponse, OrderBookResponse, Transaction}
import scorex.transaction.assets.exchange.Order

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait MatcherUtils {


  def waitForAssetBalance(node: Node, asset: String, expectedBalance: Long): Unit =
    Await.result(
      node.waitFor[AssetBalance](s"asset($asset) balance of ${node.address} >= $expectedBalance")
        (_.assetBalance(node.address, asset),
          _.balance >= expectedBalance, 5.seconds),
      3.minute
    )


  def matcherPlaceOrder(matcherNode: Node, order: Order): (String, String) = {
    val futureResult = matcherNode.placeOrder(order)

    val result = Await.result(futureResult, 1.minute)

    (result.message.id, result.status)
  }

  def issueAsset(node: Node, name: String, amount: Long): String = {
    val description = "asset for integration tests of matcher"
    val fee = 100000000L
    val futureIssueTransaction: Future[String] = for {
      a <- node.issueAsset(node.address, name, description, amount, 0, fee, reissuable = false)
      _ <- node.waitForTransaction(a.id)
    } yield a.id

    Await.result(futureIssueTransaction, 1.minute)
  }

  def matcherCheckOrderStatus(matcherNode: Node, assetId: String, orderId: String): String = {
    val futureResult = matcherNode.getOrderStatus(assetId, orderId)

    val response = Await.result(futureResult, 1.minute)

    response.status
  }

  def waitForOrderStatus(matcherNode: Node, asset: String, orderId: String, expectedStatus: String, timeout: Duration = 1.minute): Unit = Await.result(
    matcherNode.waitFor[MatcherStatusResponse](s"order(asset=$asset, orderId=$orderId) status == $expectedStatus")
      (_.getOrderStatus(asset, orderId),
        _.status == expectedStatus, 5.seconds),
    timeout
  )

  def checkOrderStatusDontChange(matcherNode: Node, asset: String, orderId: String, expectedStatus: String, times: Int = 5, interval: FiniteDuration = 1.second): Unit = {
    def aux(rest: Int, acc: Future[Unit]): Future[Unit] = {
      if (rest == 0) acc
      else for {
        _ <- acc
        _ <- GlobalTimer.instance.schedule(Future.successful(()), interval)
        r <- matcherNode.getOrderStatus(asset, orderId)
      } yield assert(r.status == expectedStatus, s"${r.status} == $expectedStatus of $orderId")
    }

    Await.result(aux(times, Future.successful(())), times * interval + 1.second)
  }

  def matcherGetOrderBook(matcherNode: Node, assetId: String): OrderBookResponse = {
    val futureResult = matcherNode.getOrderBook(assetId)

    val result = Await.result(futureResult, 1.minute)

    result
  }

  def getBalance(node: Node): (Long, Long) = {
    val initialHeight = Await.result(node.height, 1.minute)
    Await.result(node.waitForHeight(initialHeight + 2), 2.minute)

    val balance = Await.result(node.balance(node.address), 1.minute).balance
    val height = Await.result(node.height, 1.minute)

    (balance, height)
  }

}
