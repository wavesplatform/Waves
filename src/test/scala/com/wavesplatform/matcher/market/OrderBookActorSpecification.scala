package com.wavesplatform.matcher.market

import akka.actor.{ActorSystem, Props}
import akka.persistence.inmemory.extension.{InMemoryJournalStorage, InMemorySnapshotStorage, StorageExtension}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.fixtures.RestartableActor
import com.wavesplatform.matcher.fixtures.RestartableActor.RestartActor
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.Event
import com.wavesplatform.matcher.model.{BuyLimitOrder, LimitOrder, SellLimitOrder}
import com.wavesplatform.settings.WavesSettings
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import play.api.libs.json.{JsObject, JsString}
import scorex.settings.WavesHardForkParameters
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction._
import scorex.transaction.assets.exchange.{AssetPair, OrderMatch}
import scorex.transaction.state.database.blockchain.{AssetsExtendedState, StoredState}
import scorex.transaction.state.database.state.extension._
import scorex.transaction.state.database.state.storage.{MVStoreAssetsExtendedStateStorage, MVStoreOrderMatchStorage, MVStoreStateStorage}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.concurrent.duration._

class OrderBookActorSpecification extends TestKit(ActorSystem("MatcherTest"))
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll
  with ImplicitSender
  with MatcherTestData
  with BeforeAndAfterEach
  with ScorexLogging
  with PathMockFactory {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  var eventsProbe = TestProbe()

  val pair = AssetPair(Some("BTC".getBytes), Some("WAVES".getBytes))
  val db = new MVStore.Builder().compress().open()
  val storedState = fromDBWithUnlimitedBalance(db, WavesHardForkParameters.Disabled)


  val settings = new WavesSettings(JsObject(Seq(
    "matcher" -> JsObject(
      Seq("account" -> JsString(MatcherAccount.address))
    )
  )))
  val wallet = new Wallet(None, "matcher", Option(WalletSeed))
  wallet.generateNewAccount()
  var actor = system.actorOf(Props(new OrderBookActor(pair, storedState,
    wallet, settings, stub[TransactionModule[StoredInBlock]]) with RestartableActor))


  override protected def beforeEach() = {
    val tp = TestProbe()
    tp.send(StorageExtension(system).journalStorage, InMemoryJournalStorage.ClearJournal)
    tp.expectMsg(akka.actor.Status.Success(""))
    tp.send(StorageExtension(system).snapshotStorage, InMemorySnapshotStorage.ClearSnapshots)
    tp.expectMsg(akka.actor.Status.Success(""))
    super.beforeEach()

    val transactionModule = stub[TransactionModule[StoredInBlock]]
    (transactionModule.isValid(_: Transaction, _: Long)).when(*, *).returns(true).anyNumberOfTimes()

    actor = system.actorOf(Props(new OrderBookActor(pair, storedState,
      wallet, settings, transactionModule) with RestartableActor))

    eventsProbe = TestProbe()
    system.eventStream.subscribe(eventsProbe.ref, classOf[Event])
  }

  "OrderBookActror" should {

    "place buy orders" in {
      val ord1 = buy(pair, 34118, 1583290045643L)
      val ord2 = buy(pair, 34120, 170484969L)
      val ord3 = buy(pair, 34000, 44521418496L)

      actor ! ord1
      expectMsg(OrderAccepted(ord1))
      actor ! ord2
      expectMsg(OrderAccepted(ord2))
      actor ! ord3
      expectMsg(OrderAccepted(ord3))

      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(ord2, ord1, ord3).map(LimitOrder(_))))
    }

    "place sell orders" in {
      val ord1 = sell(pair, 34110, 1583290045643L)
      val ord2 = sell(pair, 34220, 170484969L)
      val ord3 = sell(pair, 34000, 44521418496L)

      actor ! ord1
      expectMsg(OrderAccepted(ord1))
      actor ! ord2
      expectMsg(OrderAccepted(ord2))
      actor ! ord3
      expectMsg(OrderAccepted(ord3))

      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(ord3, ord1, ord2).map(LimitOrder(_))))
    }

    "sell market" in {
      val ord1 = buy(pair, 100, 10)
      val ord2 = sell(pair, 100, 10)

      actor ! ord1
      actor ! ord2
      receiveN(2)

      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))
    }

    "place buy and sell order to the order book and preserve it after restart" in {
      val ord1 = buy(pair, 100, 10)
      val ord2 = sell(pair, 150, 15)

      actor ! ord1
      actor ! ord2
      receiveN(2)

      actor ! RestartActor
      actor ! GetOrdersRequest

      expectMsg(GetOrdersResponse(Seq(ord2, ord1).map(LimitOrder(_))))
    }

    "execute partial market orders and preserve remaining after restart" in {
      val ord1 = buy(pair, 100, 10)
      val ord2 = sell(pair, 100, 15)

      actor ! ord1
      expectMsgType[OrderAccepted]
      actor ! ord2
      expectMsgType[OrderAccepted]

      actor ! RestartActor
      actor ! GetOrdersRequest

      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(ord2.price, 5, ord2))))
    }

    "execute one order fully and other partially and restore after restart" in {
      val ord1 = buy(pair, 100, 10)
      val ord2 = buy(pair, 100, 5)
      val ord3 = sell(pair, 100, 12)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      receiveN(3)

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(ord2.price, 3, ord2))))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

    }

    "match multiple best orders at once and restore after restart" in {
      val ord1 = sell(pair, 100, 10)
      val ord2 = sell(pair, 100, 5)
      val ord3 = sell(pair, 90, 5)
      val ord4 = buy(pair, 100, 19)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      actor ! ord4
      receiveN(4)

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(ord2.price, 1, ord2))))

    }

    "execute orders at different price levels" in {
      val ord1 = sell(pair, 100, 10)
      val ord2 = sell(pair, 110, 5)
      val ord3 = sell(pair, 110, 10)
      val ord4 = buy(pair, 115, 22)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      actor ! ord4
      receiveN(4)

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(ord3.price, 3, ord3))))

    }

    "place orders and restart without waiting for responce" in {
      val ord1 = sell(pair, 100, 10)
      val ord2 = buy(pair, 100, 19)

      ignoreMsg {
        case GetOrdersResponse(_) => false
        case m => true
      }

      (1 to 1000).foreach({ i =>
        actor ! ord1.copy()
      })


      actor ! RestartActor

      within(5.seconds) {
        actor ! GetOrdersRequest
        val items = expectMsgType[GetOrdersResponse].orders.map(_.order.id) //should have size 1000
      }

    }

    "order matched with invalid order should keep matching with others, invalid is removed" in {
      val transactionModule = stub[TransactionModule[StoredInBlock]]
      val ord1 = buy(pair, 100, 20)
      val ord2 = buy(pair, 5000, 1000) // should be invalid
      val ord3 = sell(pair, 100, 10)

      actor = system.actorOf(Props(new OrderBookActor(pair, storedState,
        wallet, settings, transactionModule) with RestartableActor {
        override def isValid(orderMatch: OrderMatch): Boolean = {
          if (orderMatch.buyOrder == ord2) false
          else true
        }
      }))

      ignoreNoMsg()

      actor ! ord1
      expectMsg(OrderAccepted(ord1))
      actor ! ord2
      expectMsg(OrderAccepted(ord2))
      actor ! ord3
      expectMsg(OrderAccepted(ord3))

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(100, 10, ord1))))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

    }
  }

  def fromDBWithUnlimitedBalance(mvStore: MVStore, settings: WavesHardForkParameters): StoredState = {
    val storage = new MVStoreStateStorage with MVStoreOrderMatchStorage with MVStoreAssetsExtendedStateStorage {
      override val db: MVStore = mvStore
      if (db.getStoreVersion > 0) db.rollback()
    }
    val extendedState = new AssetsExtendedState(storage)
    val incrementingTimestampValidator = new IncrementingTimestampValidator(settings, storage)
    val validators = Seq(
      extendedState,
      incrementingTimestampValidator,
      new GenesisValidator,
      new OrderMatchStoredState(storage),
      new IncludedValidator(storage, settings),
      new ActivatedValidator(settings)
    )
    new StoredState(storage, extendedState, incrementingTimestampValidator, validators, settings) {
      override def assetBalance(account: AssetAcc, atHeight: Option[Int]): Long = Long.MaxValue
    }
  }

}