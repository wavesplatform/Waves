package com.wavesplatform.matcher.history

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{TestKit, TestProbe}
import com.wavesplatform.NTPTime
import com.wavesplatform.account.{Address, KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.history.DBRecords.{OrderEventRecord, OrderRecord}
import com.wavesplatform.matcher.history.OrderHistoryActor.{SaveEvent, SaveOrder}
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.OrderStatus.{Accepted, Cancelled, Filled, PartiallyFilled}
import com.wavesplatform.matcher.model.{LimitOrder, OrderStatus}
import com.wavesplatform.matcher.settings.PostgresConnection
import com.wavesplatform.matcher.{AddressDirectory, MatcherTestData}
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType, OrderV1}
import com.wavesplatform.wallet.Wallet
import io.getquill.{PostgresJdbcContext, SnakeCase}
import monix.execution.Scheduler.Implicits.{global => scheduler}
import monix.reactive.subjects.ConcurrentSubject
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class OrderHistoryActorSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with NTPTime
    with MatcherTestData
    with PathMockFactory {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  def privateKey(seed: String): KeyPair = Wallet.generateNewAccount(seed.getBytes(), 0)

  val postgresConnection = PostgresConnection("localhost", 5435, "postgres", "docker", "org.postgresql.ds.PGSimpleDataSource")
  lazy val ctx           = new PostgresJdbcContext(SnakeCase, postgresConnection.getConfig)

  import ctx._

  def getOrders(): List[OrderRecord]      = ctx.run(querySchema[OrderRecord]("orders", _.id           -> "id"))
  def getEvents(): List[OrderEventRecord] = ctx.run(querySchema[OrderEventRecord]("events", _.orderId -> "order_id"))

  val assetId    = ByteStr("asset".getBytes)
  val matcherFee = 30000L

  val buyWavesOrder   = getOrder("test", OrderType.BUY, 300L, 1L)
  val sellWavesOrder1 = getOrder("test1", OrderType.SELL, 100L, 2L)
  val sellWavesOrder2 = getOrder("test2", OrderType.SELL, 100L, 3L)
  val sellWavesOrder3 = getOrder("test3", OrderType.SELL, 100L, 4L)

  val buyWavesOrderCancelled = getOrder("test", OrderType.BUY, 300L, 5L)

  val buyWavesOrderFilledAndCancelled = getOrder("test", OrderType.BUY, 300L, 6L)
  val sellWavesOrder4                 = getOrder("test1", OrderType.SELL, 100L, 7L)

  val sellWavesOrderFilling           = getOrder("test1", OrderType.SELL, 100L, 7L)
  val buyWavesOrderFilledAfterPlacing = getOrder("test", OrderType.BUY, 100L, 8L)

  private def testWithProbe(f: (ActorRef, TestProbe) => Unit): Unit = {

    val orderHistoryProbe = TestProbe("orderHistoryProbe")

    val addressDirectoryActor: ActorRef = system.actorOf(
      Props(
        new AddressDirectory(
          ConcurrentSubject.publish[(Address, Asset)],
          matcherSettings,
          _ => Props.empty,
          orderHistoryProbe.ref
        )
      )
    )

    f(addressDirectoryActor, orderHistoryProbe); addressDirectoryActor ! PoisonPill
  }

  private def testWithPostgres(f: ActorRef => Unit): Unit = {

    def mkAssetDescription(decimals: Int): Option[AssetDescription] = Some(
      AssetDescription(MatcherAccount, Array.emptyByteArray, Array.emptyByteArray, decimals, reissuable = false, BigInt(0), None, 0)
    )

    val blockchain = stub[Blockchain]
    (blockchain.assetDescription _).when(IssuedAsset(assetId)).returns(mkAssetDescription(8))

    val orderHistoryActor = system.actorOf(OrderHistoryActor.props(blockchain, postgresConnection))

    val addressDirectoryActor: ActorRef = system.actorOf(
      Props(
        new AddressDirectory(
          ConcurrentSubject.publish[(Address, Asset)],
          matcherSettings,
          _ => Props.empty,
          orderHistoryActor
        )
      )
    )

    f(addressDirectoryActor); addressDirectoryActor ! PoisonPill; orderHistoryActor ! PoisonPill
  }

  def getOrder(senderSeed: String, orderType: OrderType, amount: Long, timestamp: Long): LimitOrder = {
    LimitOrder(
      OrderV1(
        sender = privateKey(senderSeed),
        matcher = PublicKey("matcher".getBytes()),
        pair = AssetPair(Waves, IssuedAsset(assetId)),
        orderType = orderType,
        price = 100000000L,
        amount = amount,
        timestamp = timestamp,
        expiration = 1000L,
        matcherFee = matcherFee
      )
    )
  }

  def getOrderStatus(event: Event): OrderStatus = event match {
    case _: OrderAdded => Accepted
    case e @ OrderExecuted(submitted, _, _) =>
      val actualFilledAmount = submitted.order.amount - e.submittedRemaining.amount
      if (actualFilledAmount != submitted.order.amount) PartiallyFilled(actualFilledAmount) else Filled(actualFilledAmount)
    case OrderCanceled(limitOrder, _, _) => Cancelled(limitOrder.order.amount - limitOrder.amount)
  }

  def getFilled(event: Event): Long = event match {
    case oe: OrderExecuted => oe.executedAmount
    case _                 => 0
  }

  def orderAdded(submitted: LimitOrder): OrderAdded                            = OrderAdded(submitted, ntpTime.getTimestamp())
  def orderExecuted(submitted: LimitOrder, counter: LimitOrder): OrderExecuted = OrderExecuted(submitted, counter, ntpTime.getTimestamp())
  def orderCancelled(submitted: LimitOrder): OrderCanceled                     = OrderCanceled(submitted, false, ntpTime.getTimestamp())

  "OrderHistoryActor" should {
    "receive correct events" in testWithProbe { (addressDirectory, probe) =>
      def expectFilledAndStatus(filled: Long, orderStatus: OrderStatus): Unit = {
        val eventResult = probe.expectMsgType[SaveEvent]
        getFilled(eventResult.event) shouldBe filled
        getOrderStatus(eventResult.event) shouldBe orderStatus
      }

      addressDirectory ! orderAdded(buyWavesOrder)
      probe.expectMsgType[SaveOrder].limitOrder shouldBe buyWavesOrder

      // Executed first time
      val orderExecutedEvent1 = orderExecuted(buyWavesOrder, sellWavesOrder1)
      addressDirectory ! orderExecutedEvent1
      expectFilledAndStatus(100, PartiallyFilled(100))

      // Executed second time
      val orderExecutedEvent2 = orderExecuted(orderExecutedEvent1.submittedRemaining, sellWavesOrder2)
      addressDirectory ! orderExecutedEvent2
      expectFilledAndStatus(100, PartiallyFilled(200))

      // Executed third time
      val orderExecutedEvent3 = orderExecuted(orderExecutedEvent2.submittedRemaining, sellWavesOrder3)
      addressDirectory ! orderExecutedEvent3
      expectFilledAndStatus(100, Filled(300))

      // Order cancelled after placing
      addressDirectory ! orderAdded(buyWavesOrderCancelled)
      probe.expectMsgType[SaveOrder].limitOrder shouldBe buyWavesOrderCancelled

      addressDirectory ! orderCancelled(buyWavesOrderCancelled)
      expectFilledAndStatus(0, Cancelled(0))

      // Order cancelled after partial filling
      addressDirectory ! orderAdded(buyWavesOrderFilledAndCancelled)
      probe.expectMsgType[SaveOrder].limitOrder shouldBe buyWavesOrderFilledAndCancelled

      val cancellingOrderExecutedEvent = orderExecuted(buyWavesOrderFilledAndCancelled, sellWavesOrder4)
      addressDirectory ! cancellingOrderExecutedEvent
      expectFilledAndStatus(100, PartiallyFilled(100))

      addressDirectory ! orderCancelled(cancellingOrderExecutedEvent.submittedRemaining)
      expectFilledAndStatus(0, Cancelled(100))

      // Order executed right after placing
      addressDirectory ! orderExecuted(buyWavesOrderFilledAfterPlacing, sellWavesOrderFilling)
      probe.expectMsgType[SaveOrder].limitOrder shouldBe buyWavesOrderFilledAfterPlacing
      expectFilledAndStatus(100, Filled(100))
    }
  }

  "receive correct events" in testWithPostgres { addressDirectory =>
//    addressDirectory ! orderAdded(buyWavesOrder)
//
//    // Executed first time
//    val orderExecutedEvent1 = orderExecuted(buyWavesOrder, sellWavesOrder1)
//    addressDirectory ! orderExecutedEvent1
//
//    // Executed second time
//    val orderExecutedEvent2 = orderExecuted(orderExecutedEvent1.submittedRemaining, sellWavesOrder2)
//    addressDirectory ! orderExecutedEvent2
//
//    // Executed third time
//    val orderExecutedEvent3 = orderExecuted(orderExecutedEvent2.submittedRemaining, sellWavesOrder3)
//    addressDirectory ! orderExecutedEvent3
//
//    Thread.sleep(1500)
//    getOrders().length shouldBe 1
//    getEvents().length shouldBe 3

    (1 to 10000).foreach { idx =>
      val order = getOrder(idx.toString, OrderType.BUY, idx * 100, ntpTime.getTimestamp())
      addressDirectory ! orderAdded(order)
      addressDirectory ! orderCancelled(order)
    }

    Thread.sleep(1500)

    (10000 to 20000).foreach { idx =>
      val order = getOrder(idx.toString, OrderType.BUY, idx * 100, ntpTime.getTimestamp())
      addressDirectory ! orderAdded(order)
      addressDirectory ! orderCancelled(order)
    }

    Thread.sleep(1500)
  }
}
