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

    val pair12 = AssetPair(Some(ByteStr(Array(1))), Some(ByteStr(Array(2)))) // snapshots every 13 messages
    val pair78 = AssetPair(Some(ByteStr(Array(6))), Some(ByteStr(Array(7)))) // snapshots every 8 messages

    "forces an order book to create a snapshot" when {
      "it didn't do snapshots for a long time" when {
        "first time" in snapshotTest(pair12) { (matcherActor, probes) =>
          sendBuyOrders(matcherActor, pair12, 0 to 16)
          probes.head.expectMsg(OrderBookSnapshotUpdated(pair12, 16))
        }

        "later" in snapshotTest(pair12) { (matcherActor, probes) =>
          val probe = probes.head
          sendBuyOrders(matcherActor, pair12, 0 to 16)
          probe.expectMsg(OrderBookSnapshotUpdated(pair12, 16))

          sendBuyOrders(matcherActor, pair12, 17 to 33)
          probe.expectMsg(OrderBookSnapshotUpdated(pair12, 32))
        }

        "multiple order books" in snapshotTest(pair12, pair78) { (matcherActor, probes) =>
          val List(probe12, probe78) = probes
          sendBuyOrders(matcherActor, pair12, 0 to 8)
          probe12.expectNoMessage(200.millis)
          probe78.expectNoMessage(200.millis)

          sendBuyOrders(matcherActor, pair78, 9 to 17)
          probe12.expectNoMessage(200.millis)
          probe78.expectMsg(OrderBookSnapshotUpdated(pair78, 16))

          sendBuyOrders(matcherActor, pair12, 17 to 18)
          probe12.expectMsg(OrderBookSnapshotUpdated(pair12, 17))
          probe78.expectNoMessage(200.millis)
        }
      }

      "received a lot of messages and tries to maintain a snapshot's offset" in snapshotTest(pair12) { (matcherActor, probes) =>
        val probe = probes.head
        sendBuyOrders(matcherActor, pair12, 0 to 30)
        probe.expectMsg(OrderBookSnapshotUpdated(pair12, 16))
        probe.expectNoMessage(200.millis)

        sendBuyOrders(matcherActor, pair12, 31 to 40)
        probe.expectMsg(OrderBookSnapshotUpdated(pair12, 32))
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
          matcherSettings.copy(snapshotsInterval = 20),
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
        case SaveSnapshot =>
          val event = OrderBookSnapshotUpdated(assetPair, nr)
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
          (assetPair, matcher) => OrderBookActor.props(matcher, ???, assetPair, _ => {}, _ => {}, _ => {}, matcherSettings, txFactory, ntpTime),
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
