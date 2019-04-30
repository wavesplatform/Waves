package com.wavesplatform.matcher

import java.time.{Instant, Duration => JDuration}

import akka.actor.{Actor, Cancellable}
import akka.pattern.pipe
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.Matcher.StoreEvent
import com.wavesplatform.matcher.OrderDB.orderInfoOrdering
import com.wavesplatform.matcher.error.MatcherError
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.{LimitOrder, OrderInfo, OrderStatus, OrderValidator}
import com.wavesplatform.matcher.queue.QueueEvent
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair.assetIdStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import org.slf4j.LoggerFactory

import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap}
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class AddressActor(
    owner: Address,
    spendableBalance: Asset => Long,
    cancelTimeout: FiniteDuration,
    time: Time,
    orderDB: OrderDB,
    hasOrder: Order.Id => Boolean,
    storeEvent: StoreEvent,
) extends Actor
    with ScorexLogging {

  import AddressActor._
  import context.dispatcher

  protected override def log = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))

  private val pendingCancellation = MutableMap.empty[ByteStr, Promise[Resp]]
  private val pendingPlacement    = MutableMap.empty[ByteStr, Promise[Resp]]

  private val activeOrders  = MutableMap.empty[Order.Id, LimitOrder]
  private val openVolume    = MutableMap.empty[Asset, Long].withDefaultValue(0L)
  private val expiration    = MutableMap.empty[ByteStr, Cancellable]
  private var latestOrderTs = 0L

  private def reserve(limitOrder: LimitOrder): Unit = {
    activeOrders += limitOrder.order.id() -> limitOrder
    for ((id, b) <- limitOrder.requiredBalance if b != 0) {
      val prevBalance = openVolume(id)
      val newBalance  = prevBalance + b
      log.trace(s"${limitOrder.order.id()}, ${assetIdStr(id)}: $prevBalance + $b = $newBalance")
      openVolume += id -> newBalance
    }
  }

  private def release(orderId: ByteStr): Unit =
    for (limitOrder <- activeOrders.get(orderId); (id, b) <- limitOrder.requiredBalance if b != 0) {
      val prevBalance = openVolume(id)
      val newBalance  = prevBalance - b
      log.trace(s"${limitOrder.order.id()}, ${assetIdStr(id)}: $prevBalance - $b = $newBalance")
      openVolume += id -> newBalance
    }

  private def updateTimestamp(newTimestamp: Long): Unit = if (newTimestamp > latestOrderTs) {
    latestOrderTs = newTimestamp
  }

  private def tradableBalance(assetId: Asset): Long = spendableBalance(assetId) - openVolume(assetId)

  private val validator =
    OrderValidator.accountStateAware(owner,
                                     tradableBalance,
                                     activeOrders.size,
                                     id => activeOrders.contains(id) || orderDB.containsInfo(id) || hasOrder(id)) _

  private def handleCommands: Receive = {
    case evt: BalanceUpdated =>
      val toCancel = ordersToDelete(toSpendable(evt))
      if (toCancel.nonEmpty) {
        log.debug(s"Canceling: $toCancel")
        toCancel.foreach { x =>
          storeCanceled(x.assetPair, x.orderId)
        }
      }

    case PlaceOrder(o) =>
      pendingPlacement
        .get(o.id())
        .fold {
          log.debug(s"New order: ${o.json()}")
          validator(o) match {
            case Left(error) => Future.successful(api.OrderRejected(error))
            case Right(_) =>
              updateTimestamp(o.timestamp)
              reserve(LimitOrder(o))
              storePlaced(o)
          }
        }(_.future) pipeTo sender()

    case CancelOrder(id) =>
      pendingCancellation
        .get(id)
        .fold(activeOrders.get(id) match {
          case Some(lo) => storeCanceled(lo.order.assetPair, lo.order.id())
          case None =>
            val reason = orderDB.status(id) match {
              case OrderStatus.NotFound     => MatcherError.OrderNotFound(id)
              case OrderStatus.Cancelled(_) => MatcherError.OrderCanceled(id)
              case OrderStatus.Filled(_)    => MatcherError.OrderFull(id)
            }

            Future.successful(api.OrderCancelRejected(reason))
        })(_.future) pipeTo sender()

    case CancelAllOrders(maybePair, _) =>
      val batchCancelFutures = for {
        lo <- activeOrders.values
        if maybePair.forall(_ == lo.order.assetPair)
      } yield storeCanceled(lo.order.assetPair, lo.order.id()).map(lo.order.id() -> _)

      Future.sequence(batchCancelFutures).map(_.toMap).map(api.BatchCancelCompleted).pipeTo(sender())

    case CancelExpiredOrder(id) =>
      expiration.remove(id)
      for (lo <- activeOrders.get(id)) {
        if ((lo.order.expiration - time.correctedTime()).max(0L).millis <= ExpirationThreshold) {
          log.trace(s"Order $id expired, storing cancel event")
          storeCanceled(lo.order.assetPair, lo.order.id())
        } else {
          scheduleExpiration(lo.order)
        }
      }
  }

  private def store(id: ByteStr, event: QueueEvent, eventCache: MutableMap[ByteStr, Promise[Resp]], error: Resp): Future[Resp] = {
    val promisedResponse = Promise[api.WrappedMatcherResponse]
    eventCache += id -> promisedResponse
    storeEvent(event).transformWith {
      case Failure(e) =>
        log.error(s"Error persisting $event", e)
        Future.successful(error)
      case Success(_) => promisedResponse.future
    }
  }

  private def storeCanceled(assetPair: AssetPair, id: ByteStr): Future[Resp] =
    store(id, QueueEvent.Canceled(assetPair, id), pendingCancellation, api.OrderCancelRejected(MatcherError.CanNotPersistEvent))
  private def storePlaced(order: Order): Future[Resp] = {
    import com.wavesplatform.matcher.error._
    store(order.id(), QueueEvent.Placed(order), pendingPlacement, api.OrderRejected(MatcherError.CanNotPersistEvent))
  }

  private def confirmPlacement(order: Order): Unit = for (p <- pendingPlacement.remove(order.id())) {
    log.trace(s"Confirming placement for ${order.id()}")
    p.success(api.OrderAccepted(order))
  }

  private def handleStatusRequests: Receive = {
    case GetOrderStatus(orderId) =>
      sender() ! activeOrders.get(orderId).fold[OrderStatus](orderDB.status(orderId))(activeStatus)
    case GetOrders(maybePair, onlyActive) =>
      log.trace(s"Loading ${if (onlyActive) "active" else "all"} ${maybePair.fold("")(_.toString + " ")}orders")
      val matchingActiveOrders = (for {
        lo <- activeOrders.values
        if maybePair.forall(_ == lo.order.assetPair)
      } yield
        lo.order
          .id() -> OrderInfo(lo.order.orderType, lo.order.amount, lo.order.price, lo.order.timestamp, activeStatus(lo), lo.order.assetPair)).toSeq.sorted

      log.trace(s"Collected ${matchingActiveOrders.length} active orders")

      sender() ! (if (onlyActive) matchingActiveOrders else orderDB.loadRemainingOrders(owner, maybePair, matchingActiveOrders))
    case GetTradableBalance(pair) =>
      sender() ! Set(pair.amountAsset, pair.priceAsset).map(id => id -> tradableBalance(id)).toMap
    case GetReservedBalance =>
      sender() ! openVolume.filter(_._2 > 0).toMap
  }

  private def handleExecutionEvents: Receive = {
    case OrderAdded(submitted) if submitted.order.sender.toAddress == owner =>
      log.trace(s"OrderAdded(${submitted.order.id()})")
      updateTimestamp(submitted.order.timestamp)
      release(submitted.order.id())
      handleOrderAdded(submitted)
    case e @ OrderExecuted(submitted, counter, _) =>
      log.trace(s"OrderExecuted(${submitted.order.id()}, ${counter.order.id()}), amount = ${e.executedAmount}")
      handleOrderExecuted(e.submittedRemaining)
      handleOrderExecuted(e.counterRemaining)

    case OrderCanceled(lo, unmatchable) =>
      // submitted order gets canceled if it cannot be matched with the best counter order (e.g. due to rounding issues)
      confirmPlacement(lo.order)
      pendingCancellation.remove(lo.order.id()).foreach(_.success(api.OrderCanceled(lo.order.id())))
      if (activeOrders.contains(lo.order.id())) {
        log.trace(s"OrderCanceled(${lo.order.id()}, system=$unmatchable)")
        release(lo.order.id())
        handleOrderTerminated(lo, OrderStatus.finalStatus(lo, unmatchable))
      }
  }

  private def scheduleExpiration(order: Order): Unit = {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}")
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def handleOrderAdded(lo: LimitOrder): Unit = {
    orderDB.saveOrder(lo.order)
    reserve(lo)
    confirmPlacement(lo.order)
    scheduleExpiration(lo.order)
  }

  private def handleOrderExecuted(remaining: LimitOrder): Unit = if (remaining.order.sender.toAddress == owner) {
    updateTimestamp(remaining.order.timestamp)
    release(remaining.order.id())
    if (remaining.isValid) {
      handleOrderAdded(remaining)
    } else {
      confirmPlacement(remaining.order)
      val actualFilledAmount = remaining.order.amount - remaining.amount
      handleOrderTerminated(remaining, OrderStatus.Filled(actualFilledAmount))
    }
  }

  private def handleOrderTerminated(lo: LimitOrder, status: OrderStatus.Final): Unit = {
    log.trace(s"Order ${lo.order.id()} terminated: $status")
    orderDB.saveOrder(lo.order)
    pendingCancellation.remove(lo.order.id()).foreach(_.success(api.OrderCancelRejected(MatcherError.OrderFinalized(lo.order.id()))))
    expiration.remove(lo.order.id()).foreach(_.cancel())
    activeOrders.remove(lo.order.id())
    orderDB.saveOrderInfo(
      lo.order.id(),
      owner,
      OrderInfo(lo.order.orderType, lo.order.amount, lo.order.price, lo.order.timestamp, status, lo.order.assetPair)
    )
  }

  def receive: Receive = handleCommands orElse handleExecutionEvents orElse handleStatusRequests

  private type SpendableBalance = Map[Asset, Long]

  /**
    * @param initBalance Contains only changed assets
    */
  private def ordersToDelete(initBalance: SpendableBalance): Queue[QueueEvent.Canceled] = {
    def keepChanged(requiredBalance: Map[Asset, Long]) = requiredBalance.filter {
      case (requiredAssetId, _) => initBalance.contains(requiredAssetId)
    }

    // Now a user can have 100 active transaction maximum - easy to traverse.
    val (_, r) = activeOrders.values.toSeq
      .sortBy(_.order.timestamp)(Ordering[Long]) // Will cancel newest orders first
      .view
      .map { lo =>
        (lo.order.id(), lo.order.assetPair, keepChanged(lo.requiredBalance))
      }
      .foldLeft((initBalance, Queue.empty[QueueEvent.Canceled])) {
        case ((restBalance, toDelete), (id, assetPair, requiredBalance)) =>
          remove(restBalance, requiredBalance) match {
            case Some(updatedRestBalance) => (updatedRestBalance, toDelete)
            case None =>
              val updatedToDelete = if (pendingCancellation.contains(id)) toDelete else toDelete.enqueue(QueueEvent.Canceled(assetPair, id))
              (restBalance, updatedToDelete)
          }
      }
    r
  }

  private def toSpendable(event: BalanceUpdated): SpendableBalance = {
    val r: SpendableBalance = event.changedAssets.map(x => x -> spendableBalance(x))(collection.breakOut)
    r.withDefaultValue(0)
  }

  private def remove(from: SpendableBalance, xs: SpendableBalance): Option[SpendableBalance] =
    xs.foldLeft[Option[SpendableBalance]](Some(from)) {
      case (None, _)      => None
      case (curr, (_, 0)) => curr
      case (Some(curr), (assetId, amount)) =>
        val updatedAmount = curr.getOrElse(assetId, 0L) - amount
        if (updatedAmount < 0) None
        else Some(curr.updated(assetId, updatedAmount))
    }
}

object AddressActor {
  private val ExpirationThreshold = 50.millis

  private type Resp = api.WrappedMatcherResponse

  private def activeStatus(lo: LimitOrder): OrderStatus =
    if (lo.amount == lo.order.amount) OrderStatus.Accepted else OrderStatus.PartiallyFilled(lo.order.amount - lo.amount)

  sealed trait Command

  case class GetOrderStatus(orderId: ByteStr)                             extends Command
  case class GetOrders(assetPair: Option[AssetPair], onlyActive: Boolean) extends Command
  case class GetTradableBalance(assetPair: AssetPair)                     extends Command
  case object GetReservedBalance                                          extends Command
  case class PlaceOrder(order: Order) extends Command {
    override lazy val toString = s"PlaceOrder(${order.id()},${order.sender},${order.assetPair},${order.orderType},${order.price},${order.amount})"
  }
  case class CancelOrder(orderId: ByteStr)                             extends Command
  case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long) extends Command
  case class BalanceUpdated(changedAssets: Set[Asset])                 extends Command

  private case class CancelExpiredOrder(orderId: ByteStr)
}
