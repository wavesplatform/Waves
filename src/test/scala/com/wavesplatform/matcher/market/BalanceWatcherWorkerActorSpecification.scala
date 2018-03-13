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
import scorex.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration.DurationInt

class BalanceWatcherWorkerActorSpecification
    extends TestKit(ActorSystem("BalanceWatcherWorkerActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender {

  private val fooAddr = addr("foo")
  private val barAddr = addr("bar")
  private val bazAddr = addr("baz")

  "BalanceWatcherWorkerActor" when {
    "becomes working" should {
      "ask active orders from the history" in withActors { actors =>
        actors.balanceWatcher ! noChanges(fooAddr)
        actors.history.expectMsgPF(hint = "ask active orders") {
          case GetActiveOrdersByAddress(_, `fooAddr`, _) => true
        }
      }

      "increment an ask id" in withActors { actors =>
        actors.balanceWatcher ! noChanges(fooAddr)
        actors.history.expectMsg(GetActiveOrdersByAddress(0, fooAddr, Set.empty))
        actors.history.send(actors.balanceWatcher, GetActiveOrdersByAddressResponse(0, fooAddr, Seq.empty))

        actors.balanceWatcher ! noChanges(barAddr)
        actors.history.expectMsg(GetActiveOrdersByAddress(1, barAddr, Set.empty))
      }

      "stash all incoming balance changes and then become working again" in withActors { actors =>
        actors.balanceWatcher ! noChanges(fooAddr)
        actors.history.expectMsgType[GetActiveOrdersByAddress]

        actors.balanceWatcher ! noChanges(barAddr)
        actors.balanceWatcher ! noChanges(bazAddr)

        actors.history.send(actors.balanceWatcher, GetActiveOrdersByAddressResponse(0, fooAddr, Seq.empty))
        actors.history.expectMsg(GetActiveOrdersByAddress(1, barAddr, Set.empty))
        actors.history.expectMsg(GetActiveOrdersByAddress(1, bazAddr, Set.empty))
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
        actors.balanceWatcher ! noChanges(fooAddr)
        actors.history.send(actors.balanceWatcher, brokenOrderResponse)

        actors.matcher.expectMsg(ForceCancelOrder(brokenOrder.assetPair, brokenOrder.idStr()))
      }

      "send a cancel request to the history" in withActors { actors =>
        actors.balanceWatcher ! noChanges(fooAddr)

        actors.history.expectMsgType[GetActiveOrdersByAddress]
        actors.history.send(actors.balanceWatcher, brokenOrderResponse)

        actors.history.expectMsg(ForceCancelOrderFromHistory(brokenOrder.idStr()))
      }

      def brokenOrderResponse = GetActiveOrdersByAddressResponse(0, fooAddr, Seq(LimitOrder(brokenOrder)))
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
        actors.balanceWatcher ! BalanceChanged(Map(fooAddr -> BalanceChanged.Changes(Portfolio(1000, LeaseInfo(0, 0), Map.empty), Set(None))))
        actors.history.send(actors.balanceWatcher, GetActiveOrdersByAddressResponse(0, fooAddr, Seq(LimitOrder(validOrder))))

        actors.matcher.expectNoMsg(100.millis)
      }
    }

    // @TODO decrease timeout
    "reaches a processing timeout" should {
      "become inactive" in withActors { actors =>
        actors.balanceWatcher ! noChanges(fooAddr)
        actors.history.expectMsgType[GetActiveOrdersByAddress]

        actors.history.expectNoMsg(BalanceWatcherWorkerActor.TimeoutToProcessChanges + 100.millis)

        actors.balanceWatcher ! noChanges(fooAddr)
        actors.history.expectMsgType[GetActiveOrdersByAddress]
      }
    }
  }

  private def noChanges(addr: Address) = BalanceChanged(Map(addr -> BalanceChanged.Changes(Monoid.empty[Portfolio], Set.empty)))

  private def addr(x: String): Address = PrivateKeyAccount.fromSeed(x).right.get.toAddress

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
