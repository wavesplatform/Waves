package com.wavesplatform.matcher.market

import akka.actor.{Actor, ActorRef, Props}
import com.wavesplatform.matcher.market.OrderBookActor.ForceCancelOrder
import com.wavesplatform.matcher.market.OrderHistoryActor.{ForceCancelOrderFromHistory, GetActiveOrdersByAddress, GetActiveOrdersByAddressResponse}
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.state2.Portfolio
import scorex.transaction.assets.exchange.AssetPair
import scorex.utils.ScorexLogging

class BalanceWatcherActor(matcher: ActorRef, orderHistory: ActorRef) extends Actor with ScorexLogging {

  private type OrdersToDelete   = List[(AssetPair, String)]
  private type ChangesByAddress = Map[String, Portfolio]

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[BalanceChanged])
  }

  override def receive: Receive = {
    case x@BalanceChanged(changes) =>
      log.debug(s"Received in receive: $x")
      becomeWorking(changes)
  }

  private def waitOrders(stashedChanges: ChangesByAddress, changesByAddress: ChangesByAddress): Receive = {
    case x@GetActiveOrdersByAddressResponse(address, orders) =>
      log.debug(s"Received in waitOrders: $x")
      // just check portfolio without any arithmetic operations
      ordersToDelete(changesByAddress, address, orders) // ERROR
//        .map(Function.tupled(ForceCancelOrder))
//          .map { x =>
//            log.debug(s"Sending to matcher: $x")
//            x
//          }
        // .foreach(matcher ! _)
          .map { case (_, id) => ForceCancelOrderFromHistory(id) }
          .map { x =>
            log.debug(s"Sending to history: $x")
            x
          }
        .foreach(orderHistory ! _)

      val updated = changesByAddress - address
      if (updated.isEmpty) {
        if (stashedChanges.isEmpty) context.become(receive)
        else becomeWorking(stashedChanges)
      } else context.become(waitOrders(stashedChanges, updated))

    case x: BalanceChanged =>
      log.debug(s"Received in waitOrders: $x")
      context.become(waitOrders(replaceChanges(stashedChanges, x.changesByAddress), changesByAddress))
  }

  private def becomeWorking(changes: ChangesByAddress): Unit = {
    changes.keys.map(GetActiveOrdersByAddress).map { x => log.debug(s"Sending to orderHistory: $x"); x }.foreach(orderHistory ! _)
    context.become(waitOrders(Map.empty, changes))
  }

  private def replaceChanges(orig: ChangesByAddress, replacement: ChangesByAddress): ChangesByAddress = orig ++ replacement

  private def ordersToDelete(portfolios: Map[String, Portfolio], ownerAddress: String, orders: Seq[LimitOrder]): OrdersToDelete = {
    portfolios
      .get(ownerAddress)
      .map { portfolio =>
        val ordersByPriority = orders.sortBy(_.order.timestamp)(Ordering[Long].reverse)
        val (_, r) = ordersByPriority.foldLeft((portfolio, List.empty: OrdersToDelete)) {
          case ((restPortfolio, toDelete), limitOrder) =>
            val updatedPortfolio = restPortfolio.copy(balance = restPortfolio.balance - restPortfolio.leaseInfo.leaseOut)
              .remove(limitOrder.spentAcc.assetId, limitOrder.getSpendAmount)
              .flatMap(_.remove(None, limitOrder.remainingFee))

            log.debug(s"$restPortfolio -> $updatedPortfolio: asset = ${limitOrder.spentAcc.assetId} -> ${limitOrder.remainingAmount}, ${limitOrder.getSpendAmount}")

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

object BalanceWatcherActor {
  def props(matcher: ActorRef, orderHistory: ActorRef): Props = Props(new BalanceWatcherActor(matcher, orderHistory))
}
