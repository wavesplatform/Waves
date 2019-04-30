package com.wavesplatform.matcher.history

import akka.actor.{Actor, ActorRef, Props}
import com.wavesplatform.matcher.history.DBRecords.{EventRecord, OrderRecord, Record}
import com.wavesplatform.matcher.history.HistoryRouter.{SaveEvent, SaveOrder}
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.{LimitOrder, MatcherModel}
import com.wavesplatform.matcher.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import io.getquill.{PostgresJdbcContext, SnakeCase}

object HistoryRouter {

  def props(blockchain: Blockchain, postgresConnection: PostgresConnection, orderHistorySettings: OrderHistorySettings): Props =
    Props(new HistoryRouter(blockchain, postgresConnection, orderHistorySettings))

  val eventTrade: Byte  = 0
  val eventCancel: Byte = 1

  val statusPartiallyFilled: Byte = 1
  val statusFilled: Byte          = 2
  val statusCancelled: Byte       = 3

  trait HistoryMsg {
    type R <: Record
    def createRecords(denormalizeAmountAndFee: (Long, AssetPair) => Double, denormalizePrice: (Long, AssetPair) => Double): Set[R]
  }

  final case class SaveOrder(limitOrder: LimitOrder, timestamp: Long) extends HistoryMsg {
    type R = OrderRecord
    def createRecords(denormalizeAmountAndFee: (Long, AssetPair) => Double, denormalizePrice: (Long, AssetPair) => Double): Set[R] = {
      val order = this.limitOrder.order
      Set(
        OrderRecord(
          id = order.id().toString,
          sender = order.sender.toString,
          senderPublicKey = order.senderPublicKey.toString,
          amountAssetId = order.assetPair.amountAsset.fold(AssetPair.WavesName)(_.id.toString),
          priceAssetId = order.assetPair.priceAsset.fold(AssetPair.WavesName)(_.id.toString),
          side = if (order.orderType == OrderType.BUY) 0 else 1,
          price = denormalizePrice(order.price, order.assetPair),
          amount = denormalizeAmountAndFee(order.amount, order.assetPair),
          timestamp = order.timestamp,
          expiration = order.expiration,
          fee = denormalizeAmountAndFee(order.matcherFee, order.assetPair),
          created = this.timestamp
        )
      )
    }
  }

  final case class SaveEvent(event: Event) extends HistoryMsg {

    type R = EventRecord

    def createRecords(denormalizeAmountAndFee: (Long, AssetPair) => Double, denormalizePrice: (Long, AssetPair) => Double): Set[R] = {
      this.event match {
        case _: OrderAdded => Set.empty[EventRecord]
        case e @ OrderExecuted(submitted, counter, timestamp) =>
          val assetPair = submitted.order.assetPair

          Set(submitted -> e.submittedRemainingAmount, counter -> e.counterRemainingAmount) map {
            case (limitOrder, remainingAmount) =>
              EventRecord(
                orderId = limitOrder.order.id().toString,
                eventType = eventTrade,
                timestamp = timestamp,
                price = denormalizePrice(limitOrder.order.price, assetPair),
                filled = denormalizeAmountAndFee(e.executedAmount, assetPair),
                totalFilled = denormalizeAmountAndFee(limitOrder.order.amount - remainingAmount, assetPair),
                status = if (remainingAmount != 0) statusPartiallyFilled else statusFilled
              )
          }

        case OrderCanceled(submitted, _, timestamp) =>
          Set(
            EventRecord(
              orderId = submitted.order.id().toString,
              eventType = eventCancel,
              timestamp = timestamp,
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

  private def denormalizeAmountAndFee(value: Long, pair: AssetPair): Double = MatcherModel.denormalizeAmountAndFee(value, blockchain, pair)
  private def denormalizePrice(value: Long, pair: AssetPair): Double        = MatcherModel.denormalizePrice(value, blockchain, pair)

  private lazy val ctx = new PostgresJdbcContext(SnakeCase, postgresConnection.getConfig); import ctx._

  private val ordersHistory: ActorRef =
    context.actorOf(
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
                  _.sender          -> "sender",
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

  private val eventsHistory: ActorRef =
    context.actorOf(
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
    case newOrder: SaveOrder      => ordersHistory forward newOrder
    case newOrderEvent: SaveEvent => eventsHistory forward newOrderEvent
  }
}
