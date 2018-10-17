package com.wavesplatform.it.sync.matcher

import com.typesafe.config.ConfigFactory.parseString
import com.typesafe.config.Config
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.it.sync.someAssetAmount
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.state.ByteStr.decodeBase58
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import com.wavesplatform.it.util._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

class RestOrderLimitTestSuite
    extends FreeSpec
    with NodesFromDocker
    with ReportingTestName
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure {
  import RestOrderLimitTestSuite._
  override protected def nodeConfigs: Seq[Config] = CustomConfigs

  private def matcher = nodes.head
  private def alice   = nodes(1)
  private def bob     = nodes(2)

  //TODO только для отладки. Удалить по готовности теста
  private var _orders = new ListBuffer[(String, String)]()
  def orders(order: (String, String)*): Unit = {
    val (vals, values) = (new ListBuffer[String], new ListBuffer[String])
    order.foreach(o => _orders.append((o._1, o._2)))
    val result = new StringBuilder()
    _orders.foreach(o => result.append(".replace(\"").append(o._2).append("\", \"").append(o._1).append("\")"))
    info(result.toString())
  }

  "Order History REST API methods should have limit for orders in response" in {
    val aliceAsset = alice
      .issue(alice.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L)
      .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val bobAsset = bob
      .issue(bob.address, "BobCoin", "BobCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L)
      .id
    nodes.waitForHeightAriseAndTxPresent(bobAsset)

    val alicePair = AssetPair(decodeBase58(aliceAsset).toOption, None)
    val bobPair   = AssetPair(decodeBase58(bobAsset).toOption, None)

    info("'fullOrderHistory' and 'ordersByAddress' (activeOnly=false) must return no more 'rest-order-limit' orders")
    val active0 = matcher.placeOrder(alice, alicePair, SELL, 1, 15.waves).message.id

    val active1    = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val partial1   = matcher.placeOrder(alice, alicePair, SELL, 2, 9.waves).message.id
    val filled1    = matcher.placeOrder(alice, alicePair, SELL, 1, 8.waves).message.id
    val cancelled1 = matcher.placeOrder(alice, alicePair, SELL, 1, 11.waves, 70.seconds).message.id
    val active2    = matcher.placeOrder(alice, bobPair, BUY, 1, 2.waves).message.id
    val filled2    = matcher.placeOrder(alice, bobPair, BUY, 1, 4.waves).message.id
    val partial2   = matcher.placeOrder(alice, bobPair, BUY, 2, 3.waves).message.id
    val cancelled2 = matcher.placeOrder(alice, bobPair, BUY, 1, 2.waves, 70.seconds).message.id
    orders(
      ("active0", active0),
      ("active1", active1),
      ("filled1", filled1),
      ("partial1", partial1),
      ("cancelled1", cancelled1),
      ("active2", active2),
      ("filled2", filled2),
      ("partial2", partial2),
      ("cancelled2", cancelled2)
    )

    // orders for matching Alice's orders
    matcher.placeOrder(bob, alicePair, BUY, 1, 8.waves).message.id // fill filled1
    matcher.placeOrder(bob, alicePair, BUY, 1, 9.waves).message.id // part fill partial1
    matcher.placeOrder(bob, bobPair, SELL, 1, 4.waves).message.id  // fill filled2
    matcher.placeOrder(bob, bobPair, SELL, 1, 3.waves).message.id  // part fill partial2

    matcher.waitOrderStatus(bobPair, cancelled2, "Cancelled", 2.minutes)

    val activeOrders              = Seq(partial2, active2, partial1, active1, active0)
    val allOrdersExceptTheFilled1 = activeOrders ++ Seq(cancelled2, filled2, cancelled1)
    val ordersByAlicePair         = Seq(partial1, active1, active0) ++ Seq(cancelled1, filled1)
    val ordersByBobPair           = Seq(partial2, active2) ++ Seq(cancelled2, filled2)

    matcher.fullOrderHistory(alice).map(_.id) should equal(allOrdersExceptTheFilled1)
    matcher.ordersByAddress(alice, activeOnly = false).map(_.id) should equal(allOrdersExceptTheFilled1)

    matcher.activeOrderHistory(alice).map(_.id) should equal(activeOrders)
    matcher.ordersByAddress(alice, activeOnly = true).map(_.id) should equal(activeOrders)

    matcher.orderHistoryByPair(alice, alicePair).map(_.id) should equal(ordersByAlicePair)
    matcher.orderHistoryByPair(alice, bobPair).map(_.id) should equal(ordersByBobPair)

    info("'fullOrderHistory' and 'ordersByAddress' must return all active orders, even if they are more than the limit")
    val active3 = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val active4 = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val active5 = matcher.placeOrder(alice, bobPair, BUY, 1, 2.waves).message.id
    val active6 = matcher.placeOrder(alice, bobPair, BUY, 1, 2.waves).message.id
    orders(("active3", active3), ("active4", active4), ("active5", active5), ("active6", active6))

    matcher.waitOrderStatus(bobPair, active6, "Accepted", 1.minutes)

    val activeOrdersAllNine         = Seq(active6, active5, active4, active3) ++ activeOrders
    val ordersByAlicePairWithTwoNew = Seq(active4, active3) ++ ordersByAlicePair
    val ordersByBobPairWithTwoNew   = Seq(active6, active5) ++ ordersByBobPair

    matcher.fullOrderHistory(alice).map(_.id) should equal(activeOrdersAllNine)
    matcher.ordersByAddress(alice, activeOnly = false).map(_.id) should equal(activeOrdersAllNine)

    matcher.activeOrderHistory(alice).map(_.id) should equal(activeOrdersAllNine)
    matcher.ordersByAddress(alice, activeOnly = true).map(_.id) should equal(activeOrdersAllNine)

    matcher.orderHistoryByPair(alice, alicePair).map(_.id) should equal(ordersByAlicePairWithTwoNew)
    matcher.orderHistoryByPair(alice, bobPair).map(_.id) should equal(ordersByBobPairWithTwoNew)

    info("'orderHistoryByPair' must return no more 'rest-order-limit' orders")
    val active7  = matcher.placeOrder(alice, alicePair, SELL, 1, 9.waves).message.id
    val active8  = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val active9  = matcher.placeOrder(alice, bobPair, BUY, 1, 1.waves).message.id
    val active10 = matcher.placeOrder(alice, bobPair, BUY, 1, 1.waves).message.id
    orders(("active7", active7), ("active8", active8), ("active9", active9), ("active10", active10))

    matcher.waitOrderStatus(bobPair, active10, "Accepted", 1.minutes)

    val activeOrdersAllThirteen                 = Seq(active10, active9, active8, active7) ++ activeOrdersAllNine
    val ordersByAlicePairWithTwoNewExceptOneOld = Seq(active8, active7) ++ ordersByAlicePairWithTwoNew.dropRight(1)
    val ordersByBobPairWithTwoMoreNew           = Seq(active10, active9) ++ ordersByBobPairWithTwoNew

    matcher.fullOrderHistory(alice).map(_.id) should equal(activeOrdersAllThirteen)
    matcher.ordersByAddress(alice, activeOnly = false).map(_.id) should equal(activeOrdersAllThirteen)

    matcher.activeOrderHistory(alice).map(_.id) should equal(activeOrdersAllThirteen)
    matcher.ordersByAddress(alice, activeOnly = true).map(_.id) should equal(activeOrdersAllThirteen)

    matcher.orderHistoryByPair(alice, alicePair).map(_.id) should equal(ordersByAlicePairWithTwoNewExceptOneOld)
    matcher.orderHistoryByPair(alice, bobPair).map(_.id) should equal(ordersByBobPairWithTwoMoreNew)

    info("All the methods move active orders that were filled")
    matcher.placeOrder(bob, bobPair, SELL, 1, 3.waves).message.id   // fill partial2
    matcher.placeOrder(bob, bobPair, SELL, 2, 2.waves).message.id   // fill active2, active5
    matcher.placeOrder(bob, alicePair, BUY, 2, 9.waves).message.id  // fill partial1, active7
    matcher.placeOrder(bob, alicePair, BUY, 1, 10.waves).message.id // fill active1

    matcher.waitOrderStatus(bobPair, active1, "Filled", 1.minutes)

    val activeOrdersAllSeven           = Seq(active10, active9, active8, active6, active4, active3, active0)
    val allOrdersWithOneFilled         = activeOrdersAllSeven ++ Seq(active7)
    val ordersByAlicePairWithTwoFilled = Seq(active8, active4, active3, active0) ++ Seq(active7, cancelled1, filled1, partial1)
    val ordersByBobPairWithTwoFilled   = Seq(active10, active9, active6) ++ Seq(active5, cancelled2, filled2, partial2, active2)

    matcher.fullOrderHistory(alice).map(_.id) should equal(allOrdersWithOneFilled)
    matcher.ordersByAddress(alice, activeOnly = false).map(_.id) should equal(allOrdersWithOneFilled)

    matcher.activeOrderHistory(alice).map(_.id) should equal(activeOrdersAllSeven)
    matcher.ordersByAddress(alice, activeOnly = true).map(_.id) should equal(activeOrdersAllSeven)

    matcher.orderHistoryByPair(alice, alicePair).map(_.id) should equal(ordersByAlicePairWithTwoFilled)
    matcher.orderHistoryByPair(alice, bobPair).map(_.id) should equal(ordersByBobPairWithTwoFilled)

    info("'orderHistoryByPair' must return all active orders, even if they are more than the limit")
    val active11 = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val active12 = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val active13 = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val active14 = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    val active15 = matcher.placeOrder(alice, alicePair, SELL, 1, 10.waves).message.id
    orders(
      ("active11", active11),
      ("active12", active12),
      ("active13", active13),
      ("active14", active14),
      ("active15", active15)
    )

    matcher.waitOrderStatus(bobPair, active15, "Accepted", 1.minutes)

    val activeOrdersAllTwelve          = Seq(active15, active14, active13, active12, active11) ++ activeOrdersAllSeven
    val ordersByAlicePairAllNineActive = Seq(active15, active14, active13, active12, active11) ++ ordersByAlicePairWithTwoFilled.take(4)

    matcher.fullOrderHistory(alice).map(_.id) should equal(activeOrdersAllTwelve)
    matcher.ordersByAddress(alice, activeOnly = false).map(_.id) should equal(activeOrdersAllTwelve)

    matcher.activeOrderHistory(alice).map(_.id) should equal(activeOrdersAllTwelve)
    matcher.ordersByAddress(alice, activeOnly = true).map(_.id) should equal(activeOrdersAllTwelve)

    matcher.orderHistoryByPair(alice, alicePair).map(_.id) should equal(ordersByAlicePairAllNineActive)
  }

}

object RestOrderLimitTestSuite {
  val reducedLimit                            = 8
  val configWithReducedRestOrderLimit: Config = parseString(s"""
                                                       |waves.matcher {
                                                       |  rest-order-limit=$reducedLimit
                                                       |}
    """.stripMargin)

  val CustomConfigs: Seq[Config] = Configs.map(configWithReducedRestOrderLimit.withFallback(_))
}
