package com.wavesplatform.matcher.market

import akka.actor.{ActorSystem, PoisonPill}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import cats.Monoid
import com.wavesplatform.matcher.market.OrderBookActor.ForceCancelOrder
import com.wavesplatform.matcher.market.OrderHistoryActor.{ForceCancelOrderFromHistory, GetActiveOrdersByAddress, GetActiveOrdersByAddressResponse}
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.state2.{ByteStr, LeaseInfo, Portfolio}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration.DurationInt

class BalanceWatcherWorkerActorSpecification
    extends TestKit(ActorSystem("BalanceWatcherWorkerActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender {

  "BalanceWatcherWorkerActor" when {
    "becomes working" should {
      "ask active orders from the history" in withActors { actors =>
        actors.balanceWatcher ! BalanceChanged(Map("foo" -> Monoid.empty[Portfolio]))
        actors.history.expectMsgPF(hint = "ask active orders") {
          case GetActiveOrdersByAddress(_, "foo") => true
        }
      }

      "increment an ask id" in withActors { actors =>
        actors.balanceWatcher ! BalanceChanged(Map("foo" -> Monoid.empty[Portfolio]))
        actors.history.expectMsg(GetActiveOrdersByAddress(0, "foo"))
        actors.history.send(actors.balanceWatcher, GetActiveOrdersByAddressResponse(0, "foo", Seq.empty))

        actors.balanceWatcher ! BalanceChanged(Map("bar" -> Monoid.empty[Portfolio]))
        actors.history.expectMsg(GetActiveOrdersByAddress(1, "bar"))
      }

      "stash all incoming balance changes and then become working again" in withActors { actors =>
        actors.balanceWatcher ! BalanceChanged(Map("foo" -> Monoid.empty[Portfolio]))
        actors.history.expectMsgType[GetActiveOrdersByAddress]

        actors.balanceWatcher ! BalanceChanged(Map("bar" -> Monoid.empty[Portfolio]))
        actors.balanceWatcher ! BalanceChanged(Map("baz" -> Monoid.empty[Portfolio]))

        actors.history.send(actors.balanceWatcher, GetActiveOrdersByAddressResponse(0, "foo", Seq.empty))
        actors.history.expectMsg(GetActiveOrdersByAddress(1, "bar"))
        actors.history.expectMsg(GetActiveOrdersByAddress(1, "baz"))
      }
    }

    "finds a broken order" should {
      val brokenOrder = Order(
        sender = PrivateKeyAccount("123".getBytes()),
        matcher = PublicKeyAccount("matcher".getBytes()),
        pair = AssetPair(None, Some(ByteStr.empty)),
        orderType = OrderType.BUY,
        price = 100000000L,
        amount = 100L,
        timestamp = 1L,
        expiration = 1000L,
        matcherFee = 100000L
      )

      "send a cancel request to the matcher" in withActors { actors =>
        actors.balanceWatcher ! BalanceChanged(Map("foo" -> Monoid.empty[Portfolio]))
        actors.history.send(actors.balanceWatcher, brokenOrderResponse)

        actors.matcher.expectMsg(ForceCancelOrder(brokenOrder.assetPair, brokenOrder.idStr()))
      }

      "send a cancel request to the history" in withActors { actors =>
        actors.balanceWatcher ! BalanceChanged(Map("foo" -> Monoid.empty[Portfolio]))

        actors.history.expectMsgType[GetActiveOrdersByAddress]
        actors.history.send(actors.balanceWatcher, brokenOrderResponse)

        actors.history.expectMsg(ForceCancelOrderFromHistory(brokenOrder.idStr()))
      }

      def brokenOrderResponse = GetActiveOrdersByAddressResponse(0, "foo", Seq(LimitOrder(brokenOrder)))
    }

    "don't find an invalid order" should {
      val validOrder = Order(
        sender = PrivateKeyAccount("123".getBytes()),
        matcher = PublicKeyAccount("matcher".getBytes()),
        pair = AssetPair(None, Some(ByteStr.empty)),
        orderType = OrderType.SELL,
        price = 1L,
        amount = 100L,
        timestamp = 1L,
        expiration = 1000L,
        matcherFee = 3L
      )

      "not cancel a valid order" in withActors { actors =>
        actors.balanceWatcher ! BalanceChanged(Map("foo" -> Portfolio(1000, LeaseInfo(0, 0), Map.empty)))
        actors.history.send(actors.balanceWatcher, GetActiveOrdersByAddressResponse(0, "foo", Seq(LimitOrder(validOrder))))

        actors.matcher.expectNoMsg(100.millis)
      }
    }

    "reaches a processing timeout" should {
      "become inactive" in withActors { actors =>
        actors.balanceWatcher ! BalanceChanged(Map("foo" -> Monoid.empty[Portfolio]))
        actors.history.expectMsgType[GetActiveOrdersByAddress]

        actors.history.expectNoMsg(BalanceWatcherWorkerActor.TimeoutToProcessChanges + 100.millis)

        actors.balanceWatcher ! BalanceChanged(Map("bar" -> Monoid.empty[Portfolio]))
        actors.history.expectMsgType[GetActiveOrdersByAddress]
      }
    }
  }

  private def withActors(f: TestActors => Unit): Unit = {
    val matcher    = TestProbe("matcher")
    val history    = TestProbe("history")
    val testActors = TestActors(matcher, history, TestActorRef(BalanceWatcherWorkerActor.props(matcher.ref, history.ref)))
    f(testActors)
    testActors.stop()
  }

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  private case class TestActors(matcher: TestProbe, history: TestProbe, balanceWatcher: TestActorRef[BalanceWatcherWorkerActor]) {
    def stop(): Unit = {
      balanceWatcher ! PoisonPill
      matcher.ref ! PoisonPill
      history.ref ! PoisonPill
    }
  }
}
