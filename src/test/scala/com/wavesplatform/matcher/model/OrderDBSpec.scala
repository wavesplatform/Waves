package com.wavesplatform.matcher.model

import com.wavesplatform.{NoShrink, WithDB}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.{MatcherTestData, OrderDB}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class OrderDBSpec extends FreeSpec with Matchers with WithDB with MatcherTestData with PropertyChecks with NoShrink {
  import OrderDBSpec._

  private def finalizedOrderInfoGen(o: Order): Gen[(Order, OrderInfo)] =
    for {
      filledAmount <- Gen.choose(0, o.amount)
      status       <- Gen.oneOf(LimitOrder.Filled(o.amount), LimitOrder.Cancelled(filledAmount))
    } yield o -> o.toInfo(status)

  private def finalizedOrderSeqGen(orderCount: Int): Gen[(PrivateKeyAccount, AssetPair, Seq[(Order, OrderInfo)])] =
    for {
      sender    <- accountGen
      pair      <- distinctPairGen
      orderList <- Gen.listOfN(orderCount, orderGenerator(sender, pair).flatMap(o => finalizedOrderInfoGen(o)))
    } yield (sender, pair, orderList)

  private val finalizedOrderInfoGen: Gen[(Order, OrderInfo)] = for {
    (o, _) <- orderGenerator
    result <- finalizedOrderInfoGen(o)
  } yield result

  private def test(f: OrderDB => Any): Any = f(OrderDB(matcherSettings, db))

  "Default OrderDB implementation" - {
    "stores" - {
      "order" in test { odb =>
        forAll(orderGenerator) {
          case (order, _) =>
            odb.saveOrder(order)
            odb.contains(order.id()) shouldBe true
        }
      }

      "order info for terminated orders" in test { odb =>
        forAll(finalizedOrderInfoGen) {
          case (o, oi) =>
            odb.saveOrderInfo(o.id(), o.sender, oi)
            odb.status(o.id()) shouldBe oi.status
        }
      }
    }

    "does not overwrite finalized info" in test { odb =>
      val dualFinalizedOrderInfoGen: Gen[(Order, OrderInfo, OrderInfo)] = for {
        (o, _)       <- orderGenerator
        filledAmount <- Gen.choose(0, o.amount)
        s1           <- Gen.oneOf(LimitOrder.Filled(o.amount), LimitOrder.Cancelled(filledAmount))
        s2           <- Gen.oneOf(LimitOrder.Filled(o.amount), LimitOrder.Cancelled(filledAmount))
      } yield
        (
          o,
          OrderInfo(o.orderType, o.amount, o.price, o.timestamp, s1, o.assetPair),
          OrderInfo(o.orderType, o.amount, o.price, o.timestamp, s2, o.assetPair),
        )

      forAll(dualFinalizedOrderInfoGen) {
        case (o, oi1, oi2) =>
          odb.saveOrderInfo(o.id(), o.sender, oi1)
          odb.saveOrderInfo(o.id(), o.sender, oi2)

          odb.status(o.id()) shouldBe oi1.status
      }
    }

    "does not store non-final order info" in test { odb =>
      val nonFinalOrderInfoGen: Gen[(Order, OrderInfo)] = for {
        (o, _)       <- orderGenerator
        filledAmount <- Gen.choose(0, o.amount)
        status       <- Gen.oneOf(LimitOrder.PartiallyFilled(filledAmount), LimitOrder.Accepted)
      } yield o -> OrderInfo(o.orderType, o.amount, o.price, o.timestamp, status, o.assetPair)

      forAll(nonFinalOrderInfoGen) {
        case (o, oi) =>
          an[IllegalArgumentException] should be thrownBy {
            odb.saveOrderInfo(o.id(), o.sender, oi)
          }

          odb.status(o.id()) shouldBe LimitOrder.NotFound
      }
    }

    "loads remaining orders when limits are not exceeded" in test { odb =>
      forAll(finalizedOrderSeqGen(20)) {
        case (sender, pair, orders) =>
          for ((o, i) <- orders) {
            odb.saveOrder(o)
            odb.saveOrderInfo(o.id(), o.sender, i)
          }

          val tuples = odb.loadRemainingOrders(sender, Some(pair), Seq.empty)
          tuples should contain allElementsOf orders.map { case (o, i) => o.id() -> i }
      }
    }

    "does not load more orders when there are too many active orders" in {
      val odb = OrderDB(matcherSettings.copy(maxOrdersPerRequest = 30), db)
      val paramGen = for {
        (sender, pair, finalizedOrders) <- finalizedOrderSeqGen(20)
        activeOrders                    <- Gen.listOfN(20, orderGenerator(sender, pair))
      } yield (sender, pair, activeOrders.map(o => o.id() -> o.toInfo(LimitOrder.Accepted)), finalizedOrders)

      forAll(paramGen) {
        case (sender, pair, active, finalized) =>
          for ((o, i) <- finalized) {
            odb.saveOrder(o)
            odb.saveOrderInfo(o.id(), o.sender, i)
          }

          val loadedOrders = odb.loadRemainingOrders(sender, Some(pair), active)
          loadedOrders should contain allElementsOf active
          loadedOrders.size should be <= matcherSettings.maxOrdersPerRequest

      }
    }
  }
}

object OrderDBSpec {
  private implicit class OrderExt(val o: Order) extends AnyVal {
    def toInfo(status: LimitOrder.OrderStatus) = OrderInfo(o.orderType, o.amount, o.price, o.timestamp, status, o.assetPair)
  }
}
