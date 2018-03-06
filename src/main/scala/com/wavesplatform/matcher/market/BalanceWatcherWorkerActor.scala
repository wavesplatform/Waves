package com.wavesplatform.matcher.market

import akka.actor.{Actor, ActorRef, Cancellable, Props, ReceiveTimeout}
import com.wavesplatform.matcher.market.BalanceWatcherWorkerActor._
import com.wavesplatform.matcher.market.OrderBookActor.ForceCancelOrder
import com.wavesplatform.matcher.market.OrderHistoryActor.{ForceCancelOrderFromHistory, GetActiveOrdersByAddress, GetActiveOrdersByAddressResponse}
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.state2.Portfolio
import scorex.account.Address
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.AssetPair
import scorex.utils.ScorexLogging

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class BalanceWatcherWorkerActor(matcher: ActorRef, orderHistory: ActorRef) extends Actor with ScorexLogging {

  private type OrdersToDelete   = List[(AssetPair, String)]
  private type ChangesByAddress = Map[Address, Portfolio]

  private var requestIdCounter = 0L

  override def receive: Receive = {
    case BalanceChanged(changes) => becomeWorking(changes)
  }

  private def waitOrders(taskId: Long,
                         stashedChanges: ChangesByAddress,
                         changesByAddress: ChangesByAddress,
                         waitOrdersTimeout: Cancellable): Receive = {
    case GetActiveOrdersByAddressResponse(`taskId`, address, orders) =>
      ordersToDelete(changesByAddress, address, orders)
        .foreach {
          case (pair, id) =>
            orderHistory ! ForceCancelOrderFromHistory(id)
            matcher ! ForceCancelOrder(pair, id)
        }

      val updated = changesByAddress - address
      if (updated.isEmpty) {
        if (stashedChanges.isEmpty) context.become(receive)
        else becomeWorking(stashedChanges)
      } else context.become(waitOrders(taskId, stashedChanges, updated, reschedule(waitOrdersTimeout)))

    case x: BalanceChanged =>
      log.debug(s"Got $x")
      context.become(
        waitOrders(
          taskId = taskId,
          stashedChanges = replaceChanges(stashedChanges, x.changesByAddress),
          changesByAddress = changesByAddress,
          waitOrdersTimeout = reschedule(waitOrdersTimeout)
        ))

    case _: ReceiveTimeout =>
      log.warn(s"Timeout to process orders for ${changesByAddress.size} addresses has been reached")
      if (stashedChanges.isEmpty) context.become(receive)
      else becomeWorking(stashedChanges)
  }

  private def becomeWorking(changes: ChangesByAddress): Unit = {
    val assets: Set[Option[AssetId]] = changes.flatMap { case (_, p) => p.assets.keys.map(Some(_)) }(collection.breakOut)
    changes.keys
      .map(GetActiveOrdersByAddress(requestIdCounter, _, assets + None))
      .foreach(orderHistory ! _)

    context.become(waitOrders(requestIdCounter, Map.empty, changes, reschedule(EmptyCancellable)))
    requestIdCounter += 1
  }

  private def reschedule(old: Cancellable): Cancellable = {
    import context.dispatcher

    old.cancel()
    context.system.scheduler.scheduleOnce(TimeoutToProcessChanges, self, ReceiveTimeout)
  }

  private def replaceChanges(orig: ChangesByAddress, replacement: ChangesByAddress): ChangesByAddress = orig ++ replacement

  private def ordersToDelete(portfolios: ChangesByAddress, ownerAddress: Address, orders: Seq[LimitOrder]): OrdersToDelete = {
    portfolios
      .get(ownerAddress)
      .map { portfolio =>
        val ordersByPriority = orders.sortBy(_.order.timestamp)(Ordering[Long].reverse)
        val (_, r) = ordersByPriority.foldLeft((portfolio, List.empty: OrdersToDelete)) {
          case ((restPortfolio, toDelete), limitOrder) =>
            val updatedPortfolio = restPortfolio
              .copy(balance = restPortfolio.balance - restPortfolio.leaseInfo.leaseOut)
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
  val TimeoutToProcessChanges: FiniteDuration = 1.minute

  val EmptyCancellable: Cancellable = new Cancellable {
    override def cancel(): Boolean    = true
    override def isCancelled: Boolean = true
  }

  def props(matcher: ActorRef, orderHistory: ActorRef): Props = Props(new BalanceWatcherWorkerActor(matcher, orderHistory))
}
