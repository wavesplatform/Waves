package com.wavesplatform.matcher

import akka.actor.Actor
import akka.pattern.pipe
import com.wavesplatform.account.Address
import com.wavesplatform.matcher.Matcher.StoreEvent
import com.wavesplatform.matcher.api._
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.OrderStatus
import com.wavesplatform.matcher.model.{LimitOrder, OrderValidator}
import com.wavesplatform.matcher.queue.QueueEvent
import com.wavesplatform.state.{ByteStr, Portfolio}
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

class AddressActor(
    owner: Address,
    portfolio: => Portfolio,
    maxTimestampDrift: FiniteDuration,
    cancelTimeout: FiniteDuration,
    loadOrderStatus: ByteStr => Future[OrderStatus],
    orderExists: ByteStr => Boolean,
    storeEvent: StoreEvent,
) extends Actor
    with ScorexLogging {

  import AddressActor._
  import context.dispatcher

  private val activeOrders  = mutable.AnyRefMap.empty[ByteStr, LimitOrder]
  private val openVolume    = mutable.AnyRefMap.empty[Option[AssetId], Long].withDefaultValue(0L)
  private var latestOrderTs = 0L

  private def reserve(limitOrder: LimitOrder): Unit =
    for ((id, b) <- limitOrder.requiredBalance if b != 0) {
      val newBalance = openVolume(id) + b
      log.trace(s"[$owner]${limitOrder.order.id()}: $id -> +$b ($newBalance)")
      openVolume += id -> newBalance
    }

  private def release(orderId: ByteStr): Unit =
    for (limitOrder <- activeOrders.get(orderId); (id, b) <- limitOrder.requiredBalance if b != 0) {
      val newBalance = openVolume(id) - b
      log.trace(s"[$owner]${limitOrder.order.id()}: $id -> -$b ($newBalance)")
      openVolume += id -> newBalance
    }

  private def updateOpenVolume(limitOrder: LimitOrder): Unit = {
    release(limitOrder.order.id())
    reserve(limitOrder)
  }

  private def updateTimestamp(newTimestamp: Long): Unit = if (newTimestamp > latestOrderTs) {
    latestOrderTs = newTimestamp
  }

  private def tradableBalance(assetId: Option[AssetId]): Long = {
    val p = portfolio
    assetId.fold(p.spendableBalance)(p.assets.getOrElse(_, 0L)) - openVolume(assetId)
  }

  private val validator =
    OrderValidator.accountStateAware(owner, tradableBalance, activeOrders.size, latestOrderTs - maxTimestampDrift.toMillis, orderExists) _

  private def handleCommands: Receive = {
    case PlaceOrder(o) =>
      log.debug(s"New order: ${o.json()}")
      updateTimestamp(o.timestamp)
      validator(o) match {
        case Right(_) =>
          val lo = LimitOrder(o)
          activeOrders += o.id() -> lo
          reserve(lo)
          latestOrderTs = latestOrderTs.max(lo.order.timestamp)
          storeEvent(QueueEvent.Placed(o)).pipeTo(sender())
        case Left(error) =>
          sender() ! OrderRejected(error)
      }
    case CancelOrder(id) =>
      activeOrders.get(id) match {
        case Some(lo) => storeEvent(QueueEvent.Canceled(lo.order.assetPair, id)).pipeTo(sender())
        case None     => sender() ! OrderCancelRejected(s"Order $id not found")
      }
    case CancelAllOrders(maybePair, timestamp) =>
      if ((timestamp - latestOrderTs).abs <= maxTimestampDrift.toMillis) {
        val batchCancelFutures = for {
          lo <- activeOrders.values
          if maybePair.forall(_ == lo.order.assetPair)
        } yield storeEvent(QueueEvent.Canceled(lo.order.assetPair, lo.order.id())).map(x => lo.order.id() -> x.asInstanceOf[WrappedMatcherResponse])

        Future.sequence(batchCancelFutures).map(_.toMap).map(BatchCancelCompleted).pipeTo(sender())
      } else {
        sender() ! OrderCancelRejected("Invalid timestamp")
      }
  }

  private def handleStatusRequests: Receive = {
    case GetOrderStatus(orderId) =>
      activeOrders.get(orderId) match {
        case Some(lo) =>
          sender() ! (if (lo.amount == lo.order.amount) LimitOrder.Accepted else LimitOrder.PartiallyFilled(lo.order.amount - lo.amount))
        case None =>
          loadOrderStatus(orderId) pipeTo sender()
      }
    case GetTradableBalance(pair) =>
      sender() ! Set(pair.amountAsset, pair.priceAsset).map(id => id -> tradableBalance(id)).toMap
    case GetReservedBalance =>
      sender() ! openVolume.filter(_._2 > 0).toMap
  }

  private def handleExecutionEvents: Receive = {
    case OrderAdded(submitted) if submitted.order.sender.toAddress == owner =>
      log.trace(s"OrderAdded(${submitted.order.id()})")
      updateTimestamp(submitted.order.timestamp)
      updateOpenVolume(submitted)
      activeOrders += submitted.order.id() -> submitted
    case e @ OrderExecuted(submitted, counter) =>
      log.trace(s"OrderExecuted(${submitted.order.id()}, ${counter.order.id()})")
      if (submitted.order.sender.toAddress == owner) {
        updateTimestamp(submitted.order.timestamp)
        release(submitted.order.id())
        if (e.submittedRemaining.isValid) {
          reserve(e.submittedRemaining)
          activeOrders += submitted.order.id() -> e.submittedRemaining
        } else {
          activeOrders -= submitted.order.id()
        }
      }
      if (counter.order.sender.toAddress == owner) {
        updateOpenVolume(e.counterRemaining)
        activeOrders += counter.order.id() -> e.counterRemaining
      }
    case OrderCanceled(lo, unmatchable) if activeOrders.contains(lo.order.id()) =>
      log.trace(s"OrderCanceled(${lo.order.id()}, system=$unmatchable)")
      release(lo.order.id())
      activeOrders -= lo.order.id()
  }

  def receive: Receive = handleCommands orElse handleExecutionEvents orElse handleStatusRequests
}

object AddressActor {
  sealed trait Command

  case class GetOrderStatus(orderId: ByteStr)                          extends Command
  case class GetActiveOrders(assetPair: Option[AssetPair])             extends Command
  case class GetTradableBalance(assetPair: AssetPair)                  extends Command
  case object GetReservedBalance                                       extends Command
  case class PlaceOrder(order: Order)                                  extends Command
  case class CancelOrder(orderId: ByteStr)                             extends Command
  case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long) extends Command

  private case class CancelTimedOut(orderId: ByteStr)
}
