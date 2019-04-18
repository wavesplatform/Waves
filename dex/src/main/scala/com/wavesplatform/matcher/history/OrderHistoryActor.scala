package com.wavesplatform.matcher.history

import akka.actor.{Actor, Cancellable, Props}
import com.wavesplatform.matcher.history.DBRecords._
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.{LimitOrder, MatcherModel}
import com.wavesplatform.matcher.settings.PostgresConnection
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import io.getquill.{PostgresJdbcContext, SnakeCase}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object OrderHistoryActor {

  def props(blockchain: Blockchain, postgresConnection: PostgresConnection): Props = Props(new OrderHistoryActor(blockchain, postgresConnection))

  final case class SaveOrder(limitOrder: LimitOrder, timestamp: Long)
  final case class SaveEvent(event: Event, limitOrder: LimitOrder, timestamp: Long)
  final case object StopAccumulate
}

class OrderHistoryActor(blockchain: Blockchain, postgresConnection: PostgresConnection) extends Actor {

  private lazy val ctx = new PostgresJdbcContext(SnakeCase, postgresConnection.getConfig)

  import OrderHistoryActor._
  import ctx._

  private var ordersBatchBuffer: mutable.Set[OrderRecord]      = mutable.Set.empty[OrderRecord]
  private var eventsBatchBuffer: mutable.Set[OrderEventRecord] = mutable.Set.empty[OrderEventRecord]

  private def normalizeValueByAssetDecimals(pair: AssetPair)(value: Long): Double = MatcherModel.toNormalized(value, blockchain, pair)

  private def createOrderRecord(saveOrder: SaveOrder): OrderRecord = {

    val order     = saveOrder.limitOrder.order
    val normalize = normalizeValueByAssetDecimals(order.assetPair) _

    OrderRecord(
      id = order.id().toString,
      sender = order.sender.toString,
      senderPublicKey = order.senderPublicKey.toString,
      amountAssetId = order.assetPair.amountAsset.fold(AssetPair.WavesName)(_.id.toString),
      priceAssetId = order.assetPair.priceAsset.fold(AssetPair.WavesName)(_.id.toString),
      side = if (order.orderType == OrderType.BUY) 0 else 1,
      price = normalize(order.price),
      amount = normalize(order.amount),
      timestamp = order.timestamp,
      expiration = order.expiration,
      fee = normalize(order.matcherFee),
      created = saveOrder.timestamp
    )
  }

  private def createEventRecord(saveEvent: SaveEvent): OrderEventRecord = {

    val order     = saveEvent.limitOrder.order
    val normalize = normalizeValueByAssetDecimals(order.assetPair) _

    val serializedEventType: Byte = saveEvent.event match {
      case _: OrderCanceled => 1
      case _                => 0
    }

    val filled = saveEvent.event match {
      case oe: OrderExecuted => oe.executedAmount
      case _                 => 0
    }

    val serializedOrderStatus: Byte = saveEvent.event match {
      case _: OrderAdded                  => 0
      case OrderExecuted(submitted, _, _) => if (submitted.amount != submitted.order.amount) 1 else 2
      case _: OrderCanceled               => 3
    }

    val totalFilled = saveEvent.limitOrder.order.amount - saveEvent.limitOrder.amount

    OrderEventRecord(
      orderId = order.id().toString,
      eventType = serializedEventType,
      timestamp = saveEvent.timestamp,
      price = order.price,
      filled = normalize(filled),
      totalFilled = normalize(totalFilled),
      status = serializedOrderStatus
    )
  }

  private def sendOrdersBatch(): Unit = {
    if (ordersBatchBuffer.nonEmpty) {
      ctx.run {
        liftQuery(ordersBatchBuffer).foreach { orderRecord =>
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
      ordersBatchBuffer.clear()
    }
  }

  private def sendEventsBatch(): Unit = {
    if (eventsBatchBuffer.nonEmpty) {
      ctx.run {
        liftQuery(eventsBatchBuffer).foreach { eventRecord =>
          querySchema[OrderEventRecord](
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
      eventsBatchBuffer.clear()
    }
  }

  private def sendStartAccumulatingMsg(linger: FiniteDuration): Cancellable = context.system.scheduler.scheduleOnce(linger, self, StopAccumulate)

  override def receive: Receive = awaitingOrdersOrEvents

  private def awaitingOrdersOrEvents: Receive = {
    case o: SaveOrder =>
      sendStartAccumulatingMsg(1.second)
      ordersBatchBuffer += createOrderRecord(o)
      context.become(accumulateBuffers)
    case e: SaveEvent =>
      sendStartAccumulatingMsg(1.second)
      eventsBatchBuffer += createEventRecord(e)
      context.become(accumulateBuffers)
  }

  private def accumulateBuffers: Receive = {
    case o: SaveOrder   => ordersBatchBuffer += createOrderRecord(o)
    case e: SaveEvent   => eventsBatchBuffer += createEventRecord(e)
    case StopAccumulate => sendOrdersBatch(); sendEventsBatch(); context.become(awaitingOrdersOrEvents)
  }
}
