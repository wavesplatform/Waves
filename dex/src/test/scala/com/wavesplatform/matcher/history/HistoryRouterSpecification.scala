package com.wavesplatform.matcher.history

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.wavesplatform.NTPTime
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.history.HistoryRouter.{SaveEvent, SaveOrder}
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.{LimitOrder, MatcherModel}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV1}
import com.wavesplatform.wallet.Wallet
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class HistoryRouterSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with NTPTime
    with MatcherTestData {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  def privateKey(seed: String): KeyPair = Wallet.generateNewAccount(seed.getBytes(), 0)

  val assetId    = ByteStr("asset".getBytes)
  val matcherFee = 30000L

  val assetDecimals: Byte = 8
  val wavesDecimals: Byte = 8

  val buyWavesOrder   = getOrder("test", OrderType.BUY, 300L, 1L)
  val sellWavesOrder1 = getOrder("test1", OrderType.SELL, 100L, 2L)
  val sellWavesOrder2 = getOrder("test2", OrderType.SELL, 100L, 3L)
  val sellWavesOrder3 = getOrder("test3", OrderType.SELL, 100L, 4L)

  val buyWavesOrderCancelled = getOrder("test", OrderType.BUY, 300L, 5L)

  val buyWavesOrderFilledAndCancelled = getOrder("test", OrderType.BUY, 300L, 6L)
  val sellWavesOrder4                 = getOrder("test1", OrderType.SELL, 100L, 7L)

  val sellWavesOrderFilling           = getOrder("test1", OrderType.SELL, 100L, 7L)
  val buyWavesOrderFilledAfterPlacing = getOrder("test", OrderType.BUY, 100L, 8L)

  def getOrder(senderSeed: String, orderType: OrderType, amount: Long, timestamp: Long): LimitOrder = {
    LimitOrder(
      OrderV1(
        sender = privateKey(senderSeed),
        matcher = PublicKey("matcher".getBytes()),
        pair = AssetPair(Waves, IssuedAsset(assetId)),
        orderType = orderType,
        price = Order.PriceConstant,
        amount = amount * Order.PriceConstant,
        timestamp = timestamp,
        expiration = 1000L,
        matcherFee = matcherFee
      )
    )
  }

  def orderAdded(submitted: LimitOrder): OrderAdded                            = OrderAdded(submitted, ntpTime.getTimestamp())
  def orderExecuted(submitted: LimitOrder, counter: LimitOrder): OrderExecuted = OrderExecuted(submitted, counter, ntpTime.getTimestamp())
  def orderCancelled(submitted: LimitOrder): OrderCanceled                     = OrderCanceled(submitted, false, ntpTime.getTimestamp())

  def denormalizeAmountAndFee(value: Long, pair: AssetPair): Double = MatcherModel.denormalizeAmountAndFee(value, wavesDecimals)
  def denormalizePrice(value: Long, pair: AssetPair): Double        = MatcherModel.fromNormalized(value, wavesDecimals, assetDecimals)

  case class OrderShortenedInfo(id: String, senderPublicKey: String, side: Byte, price: Double, amount: Double)
  case class EventShortenedInfo(orderId: String, eventType: Byte, filled: Double, totalFilled: Double, status: Byte)

  implicit class LimitOrderOps(limitOrder: LimitOrder) {
    def orderId: String = limitOrder.order.id().toString
    def sender: String  = limitOrder.order.senderPublicKey.toString
  }

  def getOrderInfo(orderAddedEvent: OrderAdded): OrderShortenedInfo = {
    SaveOrder(orderAddedEvent.order, orderAddedEvent.timestamp)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => OrderShortenedInfo(r.id, r.sender, r.side, r.price, r.amount))
      .head
  }

  def getEventsInfo(event: Event): Set[EventShortenedInfo] = {
    SaveEvent(event)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => EventShortenedInfo(r.orderId, r.eventType, r.filled, r.totalFilled, r.status))
  }

  "HistoryRouter" should {
    "correctly convert events to records" in {

      import HistoryRouter._

      // place big buy order
      getOrderInfo(orderAdded(buyWavesOrder)) shouldBe
        OrderShortenedInfo(buyWavesOrder.orderId, buyWavesOrder.sender, side = 0, price = 1, amount = 300)

      // place small sell order 1
      getOrderInfo(orderAdded(sellWavesOrder1)) shouldBe
        OrderShortenedInfo(sellWavesOrder1.orderId, sellWavesOrder1.sender, side = 1, price = 1, amount = 100)

      // big buy order executed first time
      val orderExecutedEvent1 = orderExecuted(buyWavesOrder, sellWavesOrder1)
      getEventsInfo(orderExecutedEvent1) shouldBe Set(
        EventShortenedInfo(buyWavesOrder.orderId, eventTrade, filled = 100, totalFilled = 100, statusPartiallyFilled),
        EventShortenedInfo(sellWavesOrder1.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place small sell order 2
      getOrderInfo(orderAdded(sellWavesOrder2)) shouldBe
        OrderShortenedInfo(sellWavesOrder2.orderId, sellWavesOrder2.sender, side = 1, price = 1, amount = 100)

      // big buy order executed second time
      val orderExecutedEvent2 = orderExecuted(orderExecutedEvent1.submittedRemaining, sellWavesOrder2)
      getEventsInfo(orderExecutedEvent2) shouldBe Set(
        EventShortenedInfo(buyWavesOrder.orderId, eventTrade, filled = 100, totalFilled = 200, statusPartiallyFilled),
        EventShortenedInfo(sellWavesOrder2.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place small sell order 3
      getOrderInfo(orderAdded(sellWavesOrder3)) shouldBe
        OrderShortenedInfo(sellWavesOrder3.orderId, sellWavesOrder3.sender, side = 1, price = 1, amount = 100)

      // big buy order executed third time and filled
      val orderExecutedEvent3 = orderExecuted(orderExecutedEvent2.submittedRemaining, sellWavesOrder3)
      getEventsInfo(orderExecutedEvent3) shouldBe Set(
        EventShortenedInfo(buyWavesOrder.orderId, eventTrade, filled = 100, totalFilled = 300, statusFilled),
        EventShortenedInfo(sellWavesOrder3.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place order and then cancel
      getOrderInfo(orderAdded(buyWavesOrderCancelled)) shouldBe
        OrderShortenedInfo(buyWavesOrderCancelled.orderId, buyWavesOrderCancelled.sender, side = 0, price = 1, amount = 300)

      getEventsInfo(orderCancelled(buyWavesOrderCancelled)) shouldBe Set(
        EventShortenedInfo(buyWavesOrderCancelled.orderId, eventCancel, filled = 0, totalFilled = 0, statusCancelled),
      )

      // place buy order
      getOrderInfo(orderAdded(buyWavesOrderFilledAndCancelled)) shouldBe
        OrderShortenedInfo(buyWavesOrderFilledAndCancelled.orderId, buyWavesOrderFilledAndCancelled.sender, side = 0, price = 1, amount = 300)

      // place sell order
      getOrderInfo(orderAdded(sellWavesOrder4)) shouldBe
        OrderShortenedInfo(sellWavesOrder4.orderId, sellWavesOrder4.sender, side = 1, price = 1, amount = 100)

      // buy order partially filled
      val cancellingOrderExecutedEvent = orderExecuted(buyWavesOrderFilledAndCancelled, sellWavesOrder4)
      getEventsInfo(cancellingOrderExecutedEvent) shouldBe Set(
        EventShortenedInfo(buyWavesOrderFilledAndCancelled.orderId, eventTrade, filled = 100, totalFilled = 100, statusPartiallyFilled),
        EventShortenedInfo(sellWavesOrder4.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // buy order cancelled
      getEventsInfo(orderCancelled(cancellingOrderExecutedEvent.submittedRemaining)) shouldBe Set(
        EventShortenedInfo(buyWavesOrderFilledAndCancelled.orderId, eventCancel, filled = 0, totalFilled = 100, statusCancelled),
      )
    }
  }
}
