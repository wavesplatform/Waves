package com.wavesplatform.matcher.market

import akka.actor.{Actor, ActorRef, Cancellable, Props, ReceiveTimeout}
import com.wavesplatform.matcher.market.BalanceWatcherWorkerActor._
import com.wavesplatform.matcher.market.OrderBookActor.ForceCancelOrder
import com.wavesplatform.matcher.market.OrderHistoryActor.{ForceCancelOrderFromHistory, GetActiveOrdersByAddress, GetActiveOrdersByAddressResponse}
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.state.LeaseBalance
import com.wavesplatform.account.Address
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.transaction.assets.exchange.AssetPair

import scala.concurrent.duration.FiniteDuration

class BalanceWatcherWorkerActor(settings: Settings, matcher: ActorRef, orderHistory: ActorRef) extends Actor with ScorexLogging {

  private type OrdersToDelete = List[(AssetPair, String)]

  private var requestIdCounter = 0L

  override def receive: Receive = {
    case x: BalanceChanged => becomeWorking(x)
  }

  private def waitOrders(taskId: Long, stashed: BalanceChanged, unprocessed: BalanceChanged, waitOrdersTimeout: Cancellable): Receive = {
    case GetActiveOrdersByAddressResponse(`taskId`, address, orders) =>
      ordersToDelete(unprocessed, address, orders)
        .foreach {
          case (pair, id) =>
            log.info(s"Order '$id' is no longer valid, canceling it")
            orderHistory ! ForceCancelOrderFromHistory(id)
            matcher ! ForceCancelOrder(pair, id)
        }

      val updated = unprocessed.copy(changes = unprocessed.changes - address)
      if (updated.isEmpty) {
        if (stashed.isEmpty) {
          context.become(receive)
        } else becomeWorking(stashed)
      } else context.become(waitOrders(taskId, stashed, updated, reschedule(waitOrdersTimeout)))

    case x: BalanceChanged =>
      context.become(
        waitOrders(
          taskId = taskId,
          stashed = replaceChanges(stashed, x),
          unprocessed = unprocessed,
          waitOrdersTimeout = reschedule(waitOrdersTimeout)
        ))

    case _: ReceiveTimeout =>
      if (stashed.isEmpty) context.become(receive)
      else becomeWorking(stashed)
  }

  private def becomeWorking(event: BalanceChanged): Unit = {
    event.changes.foreach {
      case (addr, changes) =>
        orderHistory ! GetActiveOrdersByAddress(requestIdCounter, addr, changes.changedAssets, System.currentTimeMillis())
    }

    context.become(waitOrders(requestIdCounter, BalanceChanged.empty, event, reschedule(EmptyCancellable)))
    requestIdCounter += 1
  }

  private def reschedule(old: Cancellable): Cancellable = {
    import context.dispatcher

    old.cancel()
    context.system.scheduler.scheduleOnce(settings.oneAddressProcessingTimeout, self, ReceiveTimeout)
  }

  private def replaceChanges(orig: BalanceChanged, replacement: BalanceChanged) = BalanceChanged(orig.changes ++ replacement.changes)

  private def ordersToDelete(unprocessed: BalanceChanged, ownerAddress: Address, orders: Seq[LimitOrder]): OrdersToDelete = {
    unprocessed.changes
      .get(ownerAddress)
      .map { changes =>
        val portfolio = changes.updatedPortfolio

        val ordersByPriority = orders.sortBy(_.order.timestamp)(Ordering[Long].reverse)
        val (_, r) = ordersByPriority.foldLeft((portfolio, List.empty: OrdersToDelete)) {
          case ((restPortfolio, toDelete), limitOrder) =>
            val updatedPortfolio = restPortfolio
              .copy(balance = restPortfolio.balance - restPortfolio.lease.out, lease = LeaseBalance.empty)
              .remove(limitOrder.spentAcc.assetId, limitOrder.getSpendAmount)
              .flatMap { p =>
                if (limitOrder.rcvAsset == AssetPair.WavesName && limitOrder.getReceiveAmount >= limitOrder.remainingFee) Some(p)
                else p.remove(None, limitOrder.remainingFee)
              }

            updatedPortfolio match {
              case Some(x) => (x, toDelete)
              case None =>
                (restPortfolio, (limitOrder.order.assetPair -> limitOrder.order.idStr()) :: toDelete)
            }
        }
        r
      }
      .getOrElse(List.empty)
  }

}

object BalanceWatcherWorkerActor {
  val EmptyCancellable: Cancellable = new Cancellable {
    override def cancel(): Boolean    = true
    override def isCancelled: Boolean = true
  }

  case class Settings(enable: Boolean, oneAddressProcessingTimeout: FiniteDuration)

  def props(settings: Settings, matcher: ActorRef, orderHistory: ActorRef): Props =
    Props(new BalanceWatcherWorkerActor(settings, matcher, orderHistory))
}
