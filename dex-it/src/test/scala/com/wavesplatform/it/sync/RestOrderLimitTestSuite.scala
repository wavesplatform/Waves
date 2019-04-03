package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr.decodeBase58
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

import scala.concurrent.duration._

class RestOrderLimitTestSuite extends MatcherSuiteBase {
  import RestOrderLimitTestSuite._
  override protected def nodeConfigs: Seq[Config] = CustomConfigs

  private def activeOrders: Seq[String] = {
    val activeOrders = node.activeOrderHistory(alice).map(_.id)
    node.ordersByAddress(alice, activeOnly = true).map(_.id) should equal(activeOrders)
    activeOrders
  }

  private def allOrders: Seq[String] = {
    val allOrders = node.fullOrderHistory(alice).map(_.id)
    node.ordersByAddress(alice, activeOnly = false).map(_.id) should equal(allOrders)
    allOrders
  }

  private def activeOrdersBy(pair: AssetPair, n: KeyPair = alice): Seq[String] =
    node.orderHistoryByPair(n, pair, activeOnly = true).map(_.id)

  private def allOrdersBy(pair: AssetPair, n: KeyPair = alice): Seq[String] = node.orderHistoryByPair(n, pair).map(_.id)

  markup("""Test suite checks only Alice's OrderHistory.
          |Bob places orders only for matching Alice's orders.""".stripMargin)

  "Order History REST API methods should have limit for orders in response" in {
    val aliceAsset = node
      .broadcastIssue(alice, "AliceCoin", "Test", someAssetAmount, 0, reissuable = false, issueFee, None)
      .id
    val bobAsset = node
      .broadcastIssue(bob, "BobCoin", "Test", someAssetAmount, 0, reissuable = false, issueFee, None)
      .id
    Seq(aliceAsset, bobAsset).foreach(node.waitForTransaction(_))

    val alicePair = AssetPair(IssuedAsset(decodeBase58(aliceAsset).get), Waves)
    val bobPair   = AssetPair(IssuedAsset(decodeBase58(bobAsset).get), Waves)

    info("'fullOrderHistory' and 'ordersByAddress' (activeOnly=false) must return no more 'rest-order-limit' orders")

    val active0 = node.placeOrder(alice, alicePair, SELL, 1, 15.waves, matcherFee).message.id

    val active1    = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val partial1   = node.placeOrder(alice, alicePair, SELL, 2, 9.waves, matcherFee).message.id
    val filled1    = node.placeOrder(alice, alicePair, SELL, 1, 8.waves, matcherFee).message.id
    val cancelled1 = node.placeOrder(alice, alicePair, SELL, 1, 11.waves, matcherFee).message.id
    val active2    = node.placeOrder(alice, bobPair, BUY, 1, 2.waves, matcherFee).message.id
    val filled2    = node.placeOrder(alice, bobPair, BUY, 1, 4.waves, matcherFee).message.id
    val partial2   = node.placeOrder(alice, bobPair, BUY, 2, 3.waves, matcherFee).message.id
    val cancelled2 = node.placeOrder(alice, bobPair, BUY, 1, 2.waves, matcherFee).message.id

    // orders for matching Alice's orders
    node.placeOrder(bob, alicePair, BUY, 1, 8.waves, matcherFee).message.id // fill filled1
    node.placeOrder(bob, alicePair, BUY, 1, 9.waves, matcherFee).message.id // part fill partial1
    node.placeOrder(bob, bobPair, SELL, 1, 4.waves, matcherFee).message.id  // fill filled2
    node.placeOrder(bob, bobPair, SELL, 1, 3.waves, matcherFee).message.id  // part fill partial2

    node.cancelOrder(alice, alicePair, cancelled1)
    node.cancelOrder(alice, alicePair, cancelled2)
    node.waitOrderStatus(bobPair, cancelled2, "Cancelled", 1.minutes)

    val activeOrdersAllFive       = Seq(partial2, active2, partial1, active1, active0)
    val allOrdersExceptTheFilled1 = activeOrdersAllFive ++ Seq(cancelled2, filled2, cancelled1)
    val activeOrdersByPair        = Seq(partial1, active1, active0)
    val allOrdersByPair           = activeOrdersByPair ++ Seq(cancelled1, filled1)

    activeOrders should equal(activeOrdersAllFive)
    allOrders should equal(allOrdersExceptTheFilled1)
    activeOrdersBy(alicePair) should equal(activeOrdersByPair)
    allOrdersBy(alicePair) should equal(allOrdersByPair)

    info("'fullOrderHistory' and 'ordersByAddress' must return all active orders, even if they are more than the limit")

    val active3 = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val active4 = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val active5 = node.placeOrder(alice, bobPair, BUY, 1, 2.waves, matcherFee).message.id
    val active6 = node.placeOrder(alice, bobPair, BUY, 1, 2.waves, matcherFee).message.id

    node.waitOrderStatus(bobPair, active6, "Accepted", 1.minutes)

    val activeOrdersAllNine          = Seq(active6, active5, active4, active3) ++ activeOrdersAllFive
    val activeOrdersByPairWithTwoNew = Seq(active4, active3) ++ activeOrdersByPair
    val allOrdersByPairWithTwoNew    = Seq(active4, active3) ++ allOrdersByPair

    activeOrders should (equal(allOrders) and equal(activeOrdersAllNine))
    activeOrdersBy(alicePair) should equal(activeOrdersByPairWithTwoNew)
    allOrdersBy(alicePair) should equal(allOrdersByPairWithTwoNew)

    info("'orderHistoryByPair' must return no more 'rest-order-limit' orders")

    val active7  = node.placeOrder(alice, alicePair, SELL, 1, 9.waves, matcherFee).message.id
    val active8  = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val active9  = node.placeOrder(alice, bobPair, BUY, 1, 1.waves, matcherFee).message.id
    val active10 = node.placeOrder(alice, bobPair, BUY, 1, 1.waves, matcherFee).message.id

    node.waitOrderStatus(bobPair, active10, "Accepted", 1.minutes)

    val activeOrdersAllThirteen               = Seq(active10, active9, active8, active7) ++ activeOrdersAllNine
    val activeOrdersByPairWithTwoMoreNew      = Seq(active8, active7) ++ activeOrdersByPairWithTwoNew
    val allOrdersByPairWithTwoNewExceptOneOld = Seq(active8, active7) ++ allOrdersByPairWithTwoNew.dropRight(1)

    activeOrders should (equal(allOrders) and equal(activeOrdersAllThirteen))
    activeOrdersBy(alicePair) should equal(activeOrdersByPairWithTwoMoreNew)
    allOrdersBy(alicePair) should equal(allOrdersByPairWithTwoNewExceptOneOld)

    info("all the methods move active orders that were filled")

    node.placeOrder(bob, bobPair, SELL, 1, 3.waves, matcherFee).message.id   // fill partial2
    node.placeOrder(bob, bobPair, SELL, 2, 2.waves, matcherFee).message.id   // fill active2, active5
    node.placeOrder(bob, alicePair, BUY, 2, 9.waves, matcherFee).message.id  // fill partial1, active7
    node.placeOrder(bob, alicePair, BUY, 1, 10.waves, matcherFee).message.id // fill active1

    node.waitOrderStatus(bobPair, active1, "Filled", 1.minutes)

    val activeOrdersAllSeven            = Seq(active10, active9, active8, active6, active4, active3, active0)
    val allOrdersWithOneFilled          = activeOrdersAllSeven ++ Seq(active1)
    val activeOrdersByPairWithTwoFilled = Seq(active8, active4, active3, active0)
    val allOrdersByPairWithTwoFilled    = activeOrdersByPairWithTwoFilled ++ Seq(active7, cancelled1, partial1, active1)

    activeOrders should equal(activeOrdersAllSeven)
    allOrders should equal(allOrdersWithOneFilled)
    activeOrdersBy(alicePair) should equal(activeOrdersByPairWithTwoFilled)
    allOrdersBy(alicePair) should equal(allOrdersByPairWithTwoFilled)

    info("'orderHistoryByPair' must return all active orders, even if they are more than the limit")

    val active11 = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val active12 = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val active13 = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val active14 = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id
    val active15 = node.placeOrder(alice, alicePair, SELL, 1, 10.waves, matcherFee).message.id

    node.waitOrderStatus(bobPair, active15, "Accepted", 1.minutes)

    val activeOrdersAllTwelve     = Seq(active15, active14, active13, active12, active11) ++ activeOrdersAllSeven
    val activeOrdersByPairAllNine = Seq(active15, active14, active13, active12, active11) ++ allOrdersByPairWithTwoFilled.take(4)

    activeOrders should (equal(allOrders) and equal(activeOrdersAllTwelve))
    activeOrdersBy(alicePair) should (equal(allOrdersBy(alicePair)) and equal(activeOrdersByPairAllNine))
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
