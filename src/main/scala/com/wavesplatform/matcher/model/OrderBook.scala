package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.model.MatcherModel.{Level, Price}
import com.wavesplatform.state.ByteStr

import scala.collection.immutable.TreeMap

case class OrderBook(bids: TreeMap[Price, Level[BuyLimitOrder]], asks: TreeMap[Price, Level[SellLimitOrder]]) {
  def bestBid: Option[BuyLimitOrder]  = bids.headOption.flatMap(_._2.headOption)
  def bestAsk: Option[SellLimitOrder] = asks.headOption.flatMap(_._2.headOption)

}

object OrderBook {
  val bidsOrdering: Ordering[Long] = (x: Long, y: Long) => -Ordering.Long.compare(x, y)
  val asksOrdering: Ordering[Long] = (x: Long, y: Long) => Ordering.Long.compare(x, y)

  val empty: OrderBook =
    OrderBook(TreeMap.empty[Price, Level[BuyLimitOrder]](bidsOrdering), TreeMap.empty[Price, Level[SellLimitOrder]](asksOrdering))

  import Events._

  def matchOrder(ob: OrderBook, o: LimitOrder): Event = o match {
    case oo: BuyLimitOrder =>
      if (ob.bestAsk.exists(oo.price >= _.price)) {
        OrderExecuted(o, ob.bestAsk.get)
      } else {
        OrderAdded(oo)
      }
    case oo: SellLimitOrder =>
      if (ob.bestBid.exists(oo.price <= _.price)) {
        OrderExecuted(o, ob.bestBid.get)
      } else {
        OrderAdded(oo)
      }
  }

  def cancelOrder(ob: OrderBook, orderId: ByteStr): Option[OrderCanceled] = {
    ob.bids
      .find { case (_, v) => v.exists(_.order.id().sameElements(orderId.arr)) }
      .orElse(ob.asks.find { case (_, v) => v.exists(_.order.id().sameElements(orderId.arr)) })
      .fold(Option.empty[OrderCanceled]) {
        case (_, v) =>
          Some(OrderCanceled(v.find(_.order.id().sameElements(orderId.arr)).get, unmatchable = false))
      }
  }

  private def updateCancelOrder(ob: OrderBook, limitOrder: LimitOrder): OrderBook = {
    (limitOrder match {
      case oo @ BuyLimitOrder(p, _, _, _) =>
        ob.bids.get(p).map { lvl =>
          val updatedQ = lvl.filter(_ != oo)
          ob.copy(bids = if (updatedQ.nonEmpty) {
            ob.bids + (p -> updatedQ)
          } else {
            ob.bids - p
          })
        }
      case oo @ SellLimitOrder(p, _, _, _) =>
        ob.asks.get(p).map { lvl =>
          val updatedQ = lvl.filter(_ != oo)
          ob.copy(asks = if (updatedQ.nonEmpty) {
            ob.asks + (p -> updatedQ)
          } else {
            ob.asks - p
          })
        }
    }).getOrElse(ob)
  }

  private def updateExecutedBuy(ob: OrderBook, o: BuyLimitOrder, remainingAmount: Long, remainingFee: Long) = ob.bids.get(o.price) match {
    case Some(l) =>
      val (l1, l2) = l.span(_ != o)
      val ll       = if (remainingAmount > 0) (l1 :+ o.copy(amount = remainingAmount, fee = remainingFee)) ++ l2.tail else l1 ++ l2.tail
      ob.copy(bids = if (ll.isEmpty) ob.bids - o.price else ob.bids + (o.price -> ll))
    case None => ob
  }

  private def updateExecutedSell(ob: OrderBook, o: SellLimitOrder, remainingAmount: Long, remainingFee: Long) = ob.asks.get(o.price) match {
    case Some(l) =>
      val (l1, l2) = l.span(_ != o)
      val ll       = if (remainingAmount > 0) (l1 :+ o.copy(amount = remainingAmount, fee = remainingFee)) ++ l2.tail else l1 ++ l2.tail
      ob.copy(asks = if (ll.isEmpty) ob.asks - o.price else ob.asks + (o.price -> ll))
    case None => ob
  }

  def updateState(ob: OrderBook, event: Event): OrderBook = event match {
    case OrderAdded(o: BuyLimitOrder) =>
      val orders = ob.bids.getOrElse(o.price, Vector.empty)
      ob.copy(bids = ob.bids + (o.price -> (orders :+ o)))
    case OrderAdded(o: SellLimitOrder) =>
      val orders = ob.asks.getOrElse(o.price, Vector.empty)
      ob.copy(asks = ob.asks + (o.price -> (orders :+ o)))
    case e @ OrderExecuted(_, c: BuyLimitOrder)  => updateExecutedBuy(ob, c, e.counterRemainingAmount, e.counterRemainingFee)
    case e @ OrderExecuted(_, c: SellLimitOrder) => updateExecutedSell(ob, c, e.counterRemainingAmount, e.counterRemainingFee)
    case OrderCanceled(limitOrder, _)            => updateCancelOrder(ob, limitOrder)
  }
}
