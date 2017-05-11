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
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, LeaseInfo, Portfolio}
import org.h2.mvstore.MVStore
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import scorex.transaction._
import scorex.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
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

  val pair = AssetPair(Some(ByteStr("BTC".getBytes)), Some(ByteStr("WAVES".getBytes)))
  val db = new MVStore.Builder().compress().open()
  val storedState: StateReader = stub[StateReader]
  val hugeAmount = Long.MaxValue / 2
  (storedState.accountPortfolio _).when(*).returns(Portfolio(hugeAmount, LeaseInfo.empty, Map(
    ByteStr("BTC".getBytes) -> hugeAmount,
    ByteStr("WAVES".getBytes) -> hugeAmount
  )))


  val settings = matcherSettings.copy(account = MatcherAccount.address)

  val wallet = new Wallet(None, "matcher".toCharArray, Option(WalletSeed))
  wallet.generateNewAccount()

  var actor = system.actorOf(Props(new OrderBookActor(pair, storedState,
    wallet, settings, stub[History], stub[FunctionalitySettings], stub[NewTransactionHandler]) with RestartableActor))


  override protected def beforeEach() = {
    val tp = TestProbe()
    tp.send(StorageExtension(system).journalStorage, InMemoryJournalStorage.ClearJournal)
    tp.expectMsg(akka.actor.Status.Success(""))
    tp.send(StorageExtension(system).snapshotStorage, InMemorySnapshotStorage.ClearSnapshots)
    tp.expectMsg(akka.actor.Status.Success(""))
    super.beforeEach()

    val transactionModule = stub[NewTransactionHandler]
    val history = stub[History]
    val functionalitySettings = stub[FunctionalitySettings]

    actor = system.actorOf(Props(new OrderBookActor(pair, storedState,
      wallet, settings, history, functionalitySettings, transactionModule) with RestartableActor {
      override def validate(orderMatch: ExchangeTransaction): Either[ValidationError, SignedTransaction] = Right(orderMatch)

      override def sendToNetwork(tx: SignedTransaction): Either[ValidationError, SignedTransaction] = Right(tx)
    }))

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
      val ord1 = buy(pair, 100, 10*Order.PriceConstant)
      val ord2 = buy(pair, 105, 10*Order.PriceConstant)

      actor ! ord1
      actor ! ord2
      receiveN(2)
      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(ord2.price, ord2.amount, ord2), BuyLimitOrder(ord1.price, ord1.amount, ord1))))

      val ord3 = sell(pair, 100, 10*Order.PriceConstant)
      actor ! ord3
      expectMsg(OrderAccepted(ord3))

      actor ! GetOrdersRequest
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(ord1.price, ord1.amount, ord1))))
    }

    "place buy and sell order to the order book and preserve it after restart" in {
      val ord1 = buy(pair, 100, 10*Order.PriceConstant)
      val ord2 = sell(pair, 150, 15*Order.PriceConstant)

      actor ! ord1
      actor ! ord2
      receiveN(2)

      actor ! RestartActor
      actor ! GetOrdersRequest

      expectMsg(GetOrdersResponse(Seq(ord2, ord1).map(LimitOrder(_))))
    }

    "execute partial market orders and preserve remaining after restart" in {
      val ord1 = buy(pair, 100, 10*Order.PriceConstant)
      val ord2 = sell(pair, 100, 15*Order.PriceConstant)

      actor ! ord1
      expectMsgType[OrderAccepted]
      actor ! ord2
      expectMsgType[OrderAccepted]

      actor ! RestartActor
      actor ! GetOrdersRequest

      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(ord2.price, 5*Order.PriceConstant, ord2))))
    }

    "execute one order fully and other partially and restore after restart" in {
      val ord1 = buy(pair, 100, 10*Order.PriceConstant)
      val ord2 = buy(pair, 100, 5*Order.PriceConstant)
      val ord3 = sell(pair, 100, 12*Order.PriceConstant)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      receiveN(3)

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(ord2.price, 3*Order.PriceConstant, ord2))))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

    }

    "match multiple best orders at once and restore after restart" in {
      val ord1 = sell(pair, 100, 10*Order.PriceConstant)
      val ord2 = sell(pair, 100, 5*Order.PriceConstant)
      val ord3 = sell(pair, 90, 5*Order.PriceConstant)
      val ord4 = buy(pair, 100, 19*Order.PriceConstant)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      actor ! ord4
      receiveN(4)

      actor ! RestartActor

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(ord2.price, 1*Order.PriceConstant, ord2))))

    }

    "execute orders at different price levels" in {
      val ord1 = sell(pair, 100, 10*Order.PriceConstant)
      val ord2 = sell(pair, 110, 5*Order.PriceConstant)
      val ord3 = sell(pair, 110, 10*Order.PriceConstant)
      val ord4 = buy(pair, 115, 22*Order.PriceConstant)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      actor ! ord4
      receiveN(4)

      actor ! GetBidOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder(ord3.price, 3*Order.PriceConstant, ord3))))

    }

    "place orders and restart without waiting for responce" in {
      val ord1 = sell(pair, 100, 10*Order.PriceConstant)
      val ord2 = buy(pair, 100, 19*Order.PriceConstant)

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
      val transactionModule = stub[NewTransactionHandler]
      val history = stub[History]
      val functionalitySettings = stub[FunctionalitySettings]
      val ord1 = buy(pair, 100, 20*Order.PriceConstant)
      val ord2 = buy(pair, 5000, 1000*Order.PriceConstant)
      // should be invalid
      val ord3 = sell(pair, 100, 10*Order.PriceConstant)

      actor = system.actorOf(Props(new OrderBookActor(pair, storedState,
        wallet, settings, history, functionalitySettings, transactionModule) with RestartableActor {
        override def validate(orderMatch: ExchangeTransaction): Either[ValidationError, SignedTransaction] = {
          if (orderMatch.buyOrder == ord2) Left(ValidationError.CustomError("test"))
          else Right(orderMatch)
        }

        override def sendToNetwork(tx: SignedTransaction): Either[ValidationError, SignedTransaction] = Right(tx)
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
      expectMsg(GetOrdersResponse(Seq(BuyLimitOrder(100 * Order.PriceConstant, 10*Order.PriceConstant, ord1))))

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq.empty))

    }

    "partially execute order with zero fee remaining part" in {

      val ord1 = sell(pair, 0.0006999, 1500 * Constants.UnitsInWave)
      val ord2 = sell(pair, 0.00067634, 3075248828L)
      val ord3 = buy(pair, 0.00073697, 3075363900L)

      actor ! ord1
      actor ! ord2
      actor ! ord3
      receiveN(3)

      actor ! GetAskOrdersRequest
      expectMsg(GetOrdersResponse(Seq(SellLimitOrder((0.0006999*Order.PriceConstant).toLong, 1500 * Constants.UnitsInWave, ord1))))

      actor ! GetOrderStatus(pair, ord2.idStr)
      expectMsg(GetOrderStatusResponse(LimitOrder.Filled))

      actor ! GetOrderStatus(pair, ord3.idStr)
      expectMsg(GetOrderStatusResponse(LimitOrder.Cancelled(3075248828L)))
    }
  }

}
