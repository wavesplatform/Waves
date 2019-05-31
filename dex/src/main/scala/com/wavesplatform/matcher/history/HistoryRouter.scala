package com.wavesplatform.matcher.history

import java.time.{Instant, LocalDateTime, ZoneOffset}

import akka.actor.{Actor, ActorRef, Props}
import com.wavesplatform.matcher.history.DBRecords.{EventRecord, OrderRecord, Record}
import com.wavesplatform.matcher.history.HistoryRouter.{SaveEvent, SaveOrder}
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.matcher.model.MatcherModel.Denormalization
import com.wavesplatform.matcher.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import io.getquill.{PostgresJdbcContext, SnakeCase}

object HistoryRouter {

  def props(blockchain: Blockchain, postgresConnection: PostgresConnection, orderHistorySettings: OrderHistorySettings): Props =
    Props(new HistoryRouter(blockchain, postgresConnection, orderHistorySettings))

  val eventTrade, buySide   = 0: Byte
  val eventCancel, sellSide = 1: Byte

  val statusPartiallyFilled: Byte = 1
  val statusFilled: Byte          = 2
  val statusCancelled: Byte       = 3

  trait HistoryMsg {

    type R <: Record // mapping between domain objects and database rows
    type Denormalize = (Long, AssetPair) => Double // how to convert amount, price fee to human-readable format

    def createRecords(denormalizeAmountAndFee: Denormalize, denormalizePrice: Denormalize): Set[R]
    def toLocalDateTime(timestamp: Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneOffset.UTC)
  }

  final case class SaveOrder(limitOrder: LimitOrder, timestamp: Long) extends HistoryMsg {

    type R = OrderRecord

    def createRecords(denormalizeAmountAndFee: Denormalize, denormalizePrice: Denormalize): Set[R] = {
      val order = this.limitOrder.order
      Set(
        OrderRecord(
          id = order.id().toString,
          senderAddress = order.sender.address,
          senderPublicKey = order.senderPublicKey.toString,
          amountAssetId = order.assetPair.amountAssetStr,
          priceAssetId = order.assetPair.priceAssetStr,
          side = if (order.orderType == OrderType.BUY) buySide else sellSide,
          price = denormalizePrice(order.price, order.assetPair),
          amount = denormalizeAmountAndFee(order.amount, order.assetPair),
          timestamp = toLocalDateTime(order.timestamp),
          expiration = toLocalDateTime(order.expiration),
          fee = denormalizeAmountAndFee(order.matcherFee, order.assetPair),
          created = toLocalDateTime(this.timestamp)
        )
      )
    }
  }

  final case class SaveEvent(event: Event) extends HistoryMsg {

    type R = EventRecord

    def createRecords(denormalizeAmountAndFee: Denormalize, denormalizePrice: Denormalize): Set[R] = {
      this.event match {
        case _: OrderAdded => Set.empty[EventRecord]

        case e @ OrderExecuted(submitted, counter, timestamp) =>
          val assetPair = submitted.order.assetPair

          Set(submitted -> e.submittedRemainingAmount, counter -> e.counterRemainingAmount) map {
            case (limitOrder, remainingAmount) =>
              EventRecord(
                orderId = limitOrder.order.id().toString,
                eventType = eventTrade,
                timestamp = toLocalDateTime(timestamp),
                price = denormalizePrice(limitOrder.order.price, assetPair),
                filled = denormalizeAmountAndFee(e.executedAmount, assetPair),
                totalFilled = denormalizeAmountAndFee(limitOrder.order.amount - remainingAmount, assetPair),
                status = if (remainingAmount == 0) statusFilled else statusPartiallyFilled
              )
          }

        case OrderCanceled(submitted, _, timestamp) =>
          Set(
            EventRecord(
              orderId = submitted.order.id().toString,
              eventType = eventCancel,
              timestamp = toLocalDateTime(timestamp),
              price = denormalizePrice(submitted.order.price, submitted.order.assetPair),
              filled = 0,
              totalFilled = denormalizeAmountAndFee(submitted.order.amount - submitted.amount, submitted.order.assetPair),
              status = statusCancelled
            )
          )
      }
    }
  }

  final case object StopAccumulate
}

class HistoryRouter(blockchain: Blockchain, postgresConnection: PostgresConnection, orderHistorySettings: OrderHistorySettings) extends Actor {

  private def denormalizeAmountAndFee(value: Long, pair: AssetPair): Double = Denormalization.denormalizeAmountAndFee(value, blockchain, pair)
  private def denormalizePrice(value: Long, pair: AssetPair): Double        = Denormalization.denormalizePrice(value, blockchain, pair)

  private val ctx = new PostgresJdbcContext(SnakeCase, postgresConnection.getConfig); import ctx._

  private val ordersHistory: ActorRef = context.actorOf(
    Props(
      new HistoryMessagesBatchSender[SaveOrder] {

        val batchLinger: Long  = orderHistorySettings.ordersBatchLingerMs
        val batchEntries: Long = orderHistorySettings.ordersBatchEntries

        def createAndSendBatch(batchBuffer: Iterable[SaveOrder]): Unit =
          ctx.run {
            liftQuery(batchBuffer flatMap { _.createRecords(denormalizeAmountAndFee, denormalizePrice) }) foreach { orderRecord =>
              querySchema[OrderRecord](
                "orders",
                _.id              -> "id",
                _.senderAddress   -> "sender_address",
                _.senderPublicKey -> "sender_public_key",
                _.amountAssetId   -> "amount_asset_id",
                _.priceAssetId    -> "price_asset_id",
                _.side            -> "side",
                _.price           -> "price",
                _.amount          -> "amount",
                _.timestamp       -> "timestamp",
                _.expiration      -> "expiration",
                _.fee             -> "fee",
                _.created         -> "created"
              ).insert(orderRecord).onConflictIgnore
            }
          }
      }
    ),
    name = "orders-history"
  )

  private val eventsHistory: ActorRef = context.actorOf(
    Props(
      new HistoryMessagesBatchSender[SaveEvent] {

        val batchLinger: Long  = orderHistorySettings.eventsBatchLingerMs
        val batchEntries: Long = orderHistorySettings.eventsBatchEntries

        def createAndSendBatch(batchBuffer: Iterable[SaveEvent]): Unit =
          ctx.run {
            liftQuery(batchBuffer flatMap { _.createRecords(denormalizeAmountAndFee, denormalizePrice) }) foreach { eventRecord =>
              querySchema[EventRecord](
                "events",
                _.orderId     -> "order_id",
                _.eventType   -> "event_type",
                _.timestamp   -> "timestamp",
                _.price       -> "price",
                _.filled      -> "filled",
                _.totalFilled -> "total_filled",
                _.status      -> "status"
              ).insert(eventRecord).onConflictIgnore
            }
          }
      }
    ),
    name = "events-history"
  )

  def receive: Receive = {
    case newOrder: SaveOrder => ordersHistory forward newOrder
    case newEvent: SaveEvent => eventsHistory forward newEvent
  }
}
