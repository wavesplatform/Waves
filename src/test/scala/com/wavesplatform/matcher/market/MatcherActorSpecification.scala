package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef, Kill, Props, Terminated}
import akka.testkit.{ImplicitSender, TestActorRef, TestProbe}
import com.wavesplatform.NTPTime
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.api.OrderAccepted
import com.wavesplatform.matcher.market.MatcherActor.{GetMarkets, MarketData}
import com.wavesplatform.matcher.market.MatcherActorSpecification.FailAtStartActor
import com.wavesplatform.matcher.model.ExchangeTransactionCreator
import com.wavesplatform.state.{AssetDescription, Blockchain, ByteStr}
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.{EmptyBlockchain, randomBytes}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
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

      actor ! order
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
              ob,
              (_, _) => Props(classOf[FailAtStartActor], pair),
              blockchain.assetDescription
            )
          ))

        actor ! buy(pair, 2000, 1)
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

        actor ! order1
        actor ! order2
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
  }

  private def defaultActor(ob: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]] = emptyOrderBookRefs): TestActorRef[MatcherActor] = {
    val txFactory = new ExchangeTransactionCreator(EmptyBlockchain, MatcherAccount, matcherSettings, ntpTime).createTransaction _
    waitInitialization(
      TestActorRef(
        new MatcherActor(
          ob,
          (assetPair, matcher) =>
            OrderBookActor.props(matcher, assetPair, _ => {}, _ => {}, mock[UtxPool], mock[ChannelGroup], matcherSettings, txFactory, ntpTime),
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
