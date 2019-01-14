package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef, Kill, Props, Terminated}
import akka.testkit.{ImplicitSender, TestActorRef, TestProbe}
import com.wavesplatform.NTPTime
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.OrderAccepted
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, MarketData, SaveSnapshot}
import com.wavesplatform.matcher.market.MatcherActorSpecification.FailAtStartActor
import com.wavesplatform.matcher.market.OrderBookActor.OrderBookSnapshotUpdated
import com.wavesplatform.matcher.model.ExchangeTransactionCreator
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import com.wavesplatform.state.{AssetDescription, Blockchain, ByteStr}
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.{EmptyBlockchain, randomBytes}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration.DurationInt

class MatcherActorSpecification
    extends MatcherSpec("MatcherActor")
    with MatcherTestData
    with BeforeAndAfterEach
    with PathMockFactory
    with ImplicitSender
    with Eventually
    with NTPTime {

  private val blockchain: Blockchain = stub[Blockchain]
  (blockchain.assetDescription _)
    .when(*)
    .returns(Some(AssetDescription(PrivateKeyAccount(Array.empty), "Unknown".getBytes, Array.emptyByteArray, 8, reissuable = false, 1, None, 0)))
    .anyNumberOfTimes()

  "MatcherActor" should {
    "return all open markets" in {
      val actor = defaultActor()

      val pair  = AssetPair(randomAssetId, randomAssetId)
      val order = buy(pair, 2000, 1)

      (blockchain.accountScript _)
        .when(order.sender.toAddress)
        .returns(None)

      (blockchain.accountScript _)
        .when(order.matcherPublicKey.toAddress)
        .returns(None)

      actor ! wrap(order)
      expectMsg(OrderAccepted(order))

      actor ! GetMarkets

      expectMsgPF() {
        case s @ Seq(MarketData(_, "Unknown", "Unknown", _, _, _)) =>
          s.size shouldBe 1
      }
    }

    "mark an order book as failed" when {
      "it crashes at start" in {
        val pair = AssetPair(randomAssetId, randomAssetId)
        val ob   = emptyOrderBookRefs
        val actor = waitInitialization(
          TestActorRef(
            new MatcherActor(
              matcherSettings,
              (_, _, _) => (),
              ob,
              (_, _) => Props(new FailAtStartActor(pair)),
              blockchain.assetDescription
            )
          ))

        actor ! wrap(buy(pair, 2000, 1))
        eventually { ob.get()(pair) shouldBe 'left }
      }

      "it crashes during the work" in {
        val ob    = emptyOrderBookRefs
        val actor = defaultActor(ob)

        val a1, a2, a3 = randomAssetId

        val pair1  = AssetPair(a1, a2)
        val order1 = buy(pair1, 2000, 1)

        val pair2  = AssetPair(a2, a3)
        val order2 = buy(pair2, 2000, 1)

        actor ! wrap(order1)
        actor ! wrap(order2)
        receiveN(2)

        ob.get()(pair1) shouldBe 'right
        ob.get()(pair2) shouldBe 'right

        val toKill = actor.getChild(List(OrderBookActor.name(pair1)).iterator)

        val probe = TestProbe()
        probe.watch(toKill)
        toKill.tell(Kill, actor)
        probe.expectMsgType[Terminated]

        ob.get()(pair1) shouldBe 'left
      }
    }

    "delete order books" is pending
    "forward new orders to order books" is pending

    // snapshotOffset == 17
    val pair23 = AssetPair(Some(ByteStr(Array(1))), Some(ByteStr(Array(2)))) // key = 2-3, snapshot offset = 9: 9, 26, 43, ...
    val pair45 = AssetPair(Some(ByteStr(Array(3))), Some(ByteStr(Array(4)))) // key = 4-5, snapshot offset = 12: 12, 29, 46, ...

    "force an order book to create a snapshot" when {
      "it didn't do snapshots for a long time" when {
        "first time" in snapshotTest(pair23) { (matcherActor, probes) =>
          sendBuyOrders(matcherActor, pair23, 0 to 9)
          probes.head.expectMsg(OrderBookSnapshotUpdated(pair23, 9))
        }

        "later" in snapshotTest(pair23) { (matcherActor, probes) =>
          val probe = probes.head
          sendBuyOrders(matcherActor, pair23, 0 to 10)
          probe.expectMsg(OrderBookSnapshotUpdated(pair23, 9))

          sendBuyOrders(matcherActor, pair23, 10 to 28)
          probe.expectMsg(OrderBookSnapshotUpdated(pair23, 26))
        }

        "multiple order books" in snapshotTest(pair23, pair45) { (matcherActor, probes) =>
          val List(probe23, probe45) = probes
          sendBuyOrders(matcherActor, pair23, 0 to 1)
          sendBuyOrders(matcherActor, pair45, 2 to 3)

          probe23.expectNoMessage(200.millis)
          probe45.expectNoMessage(200.millis)

          sendBuyOrders(matcherActor, pair45, 4 to 10)
          probe23.expectMsg(OrderBookSnapshotUpdated(pair23, 9))
          probe45.expectNoMessage(200.millis)

          sendBuyOrders(matcherActor, pair23, 11 to 14)
          probe23.expectNoMessage(200.millis)
          probe45.expectMsg(OrderBookSnapshotUpdated(pair45, 12))
        }
      }

      "received a lot of messages and tries to maintain a snapshot's offset" in snapshotTest(pair23) { (matcherActor, probes) =>
        val probe = probes.head
        sendBuyOrders(matcherActor, pair23, 0 to 30)
        probe.expectMsg(OrderBookSnapshotUpdated(pair23, 9))

        // OrderBookSnapshotUpdated(pair23, 26) is ignored in OrderBookActor, because it's waiting for SaveSnapshotSuccess of 9 from SnapshotStore.
        probe.expectNoMessage(200.millis)

        sendBuyOrders(matcherActor, pair23, 31 to 45)
        probe.expectMsg(OrderBookSnapshotUpdated(pair23, 43))
        probe.expectNoMessage(200.millis)
      }
    }
  }

  private def sendBuyOrders(actor: ActorRef, assetPair: AssetPair, indexes: Range): Unit = {
    val ts = System.currentTimeMillis()
    indexes.foreach { i =>
      actor ! wrap(i, buy(assetPair, amount = 1000, price = 1, ts = Some(ts + i)))
    }
  }

  /**
    * @param f (MatcherActor, TestProbe) => Any
    */
  private def snapshotTest(assetPairs: AssetPair*)(f: (ActorRef, List[TestProbe]) => Any): Any = {
    val r = assetPairs.map(fakeOrderBookActor).toList
    val actor = waitInitialization(
      TestActorRef(
        new MatcherActor(
          matcherSettings.copy(snapshotsInterval = 17),
          (_, _, _) => (),
          emptyOrderBookRefs,
          (assetPair, _) => {
            val idx = assetPairs.indexOf(assetPair)
            if (idx < 0) throw new RuntimeException(s"Can't find $assetPair in $assetPairs")
            r(idx)._1
          },
          blockchain.assetDescription
        )
      ))

    f(actor, r.map(_._2))
  }

  private def fakeOrderBookActor(assetPair: AssetPair): (Props, TestProbe) = {
    val probe = TestProbe()
    val props = Props(new Actor {
      import context.dispatcher
      private var nr = -1L

      override def receive: Receive = {
        case x: QueueEventWithMeta if x.offset > nr => nr = x.offset
        case SaveSnapshot(globalNr) =>
          val event = OrderBookSnapshotUpdated(assetPair, globalNr)
          context.system.scheduler.scheduleOnce(200.millis) {
            context.parent ! event
            probe.ref ! event
          }
      }
      context.parent ! OrderBookSnapshotUpdated(assetPair, 0)
    })

    (props, probe)
  }

  private def defaultActor(ob: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]] = emptyOrderBookRefs): TestActorRef[MatcherActor] = {
    val txFactory = new ExchangeTransactionCreator(EmptyBlockchain, MatcherAccount, matcherSettings).createTransaction _

    waitInitialization(
      TestActorRef(
        new MatcherActor(
          matcherSettings,
          (_, _, _) => (),
          ob,
          (assetPair, matcher) =>
            OrderBookActor.props(matcher, TestProbe().ref, assetPair, _ => {}, _ => {}, _ => {}, matcherSettings, txFactory, ntpTime),
          blockchain.assetDescription
        )
      ))
  }

  private def waitInitialization(x: TestActorRef[MatcherActor]): TestActorRef[MatcherActor] = eventually(timeout(1.second)) {
    x.underlyingActor.recoveryFinished shouldBe true
    x
  }
  private def emptyOrderBookRefs             = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private def randomAssetId: Option[AssetId] = Some(ByteStr(randomBytes()))
}

object MatcherActorSpecification {
  private class FailAtStartActor(pair: AssetPair) extends Actor {
    throw new RuntimeException("I don't want to work today")
    override def receive: Receive = Actor.emptyBehavior
  }
}
