package com.wavesplatform.matcher.market

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.Events.ExchangeTransactionCreated
import com.wavesplatform.matcher.settings.MatcherSettings.ExchangeTransactionBroadcastSettings
import com.wavesplatform.settings.loadConfig
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, ExchangeTransactionV2, Order}
import com.wavesplatform.utils.Time
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class ExchangeTransactionBroadcastActorSpecification
    extends MatcherSpec("ExchangeTransactionBroadcastActor")
    with MatcherTestData
    with BeforeAndAfterEach
    with PathMockFactory
    with ImplicitSender
    with Eventually {

  implicit override lazy val system: ActorSystem = ActorSystem(
    actorSystemName,
    loadConfig(ConfigFactory.empty())
  )

  private val pair = AssetPair(IssuedAsset(Array.emptyByteArray), Waves)

  "ExchangeTransactionBroadcastActor" should {
    "broadcast a transaction when receives it" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      defaultActor(ntpTime, isConfirmed = _ => false, broadcast = broadcasted = _)

      val event = sampleEvent()
      system.eventStream.publish(event)
      eventually {
        broadcasted shouldBe Seq(event.tx)
      }
    }

    "broadcast a transaction in next period if it wasn't confirmed" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor       = defaultActor(ntpTime, isConfirmed = _ => false, broadcast = broadcasted = _)

      val event = sampleEvent()
      system.eventStream.publish(event)
      eventually {
        broadcasted should not be empty
      }
      broadcasted = Seq.empty

      // Will be re-sent on second call
      actor ! ExchangeTransactionBroadcastActor.Send
      actor ! ExchangeTransactionBroadcastActor.Send
      eventually {
        broadcasted shouldBe Seq(event.tx)
      }
    }

    "doesn't broadcast a transaction if it was confirmed" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor       = defaultActor(ntpTime, isConfirmed = _ => true, broadcast = broadcasted = _)

      val event = sampleEvent()
      system.eventStream.publish(event)
      eventually {
        broadcasted should not be empty
      }

      actor ! ExchangeTransactionBroadcastActor.Send
      eventually {
        broadcasted shouldBe empty
      }
    }

    "doesn't broadcast an expired transaction" in {
      var broadcasted = Seq.empty[ExchangeTransaction]
      val actor       = defaultActor(ntpTime, isConfirmed = _ => true, broadcast = broadcasted = _)

      val event = sampleEvent(500.millis)
      system.eventStream.publish(event)
      eventually {
        broadcasted should not be empty
      }

      actor ! ExchangeTransactionBroadcastActor.Send
      eventually {
        broadcasted shouldBe empty
      }
    }
  }

  private def defaultActor(time: Time,
                           isConfirmed: ExchangeTransaction => Boolean,
                           broadcast: Seq[ExchangeTransaction] => Unit): TestActorRef[ExchangeTransactionBroadcastActor] = TestActorRef(
    new ExchangeTransactionBroadcastActor(
      settings = ExchangeTransactionBroadcastSettings(
        broadcastUntilConfirmed = true,
        interval = 1.minute,
        maxPendingTime = 5.minute
      ),
      time = time,
      _ => true,
      isConfirmed = isConfirmed,
      broadcast = broadcast
    )
  )

  private def sampleEvent(expiration: FiniteDuration = 1.day): ExchangeTransactionCreated = {
    val ts = ntpTime.getTimestamp()
    ExchangeTransactionCreated(
      ExchangeTransactionV2
        .create(
          buyOrder = Order.buy(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = pair,
            amount = 100,
            price = 6000000L,
            timestamp = ts,
            expiration = ts + expiration.toMillis,
            matcherFee = 100
          ),
          sellOrder = Order.sell(
            sender = KeyPair(Array.emptyByteArray),
            matcher = KeyPair(Array.emptyByteArray),
            pair = pair,
            amount = 100,
            price = 6000000L,
            timestamp = ts,
            expiration = ts + expiration.toMillis,
            matcherFee = 100
          ),
          amount = 100,
          price = 6000000L,
          buyMatcherFee = 0L,
          sellMatcherFee = 0L,
          fee = 300000L,
          timestamp = ts,
          proofs = Proofs.empty
        )
        .explicitGet()
    )
  }
}
