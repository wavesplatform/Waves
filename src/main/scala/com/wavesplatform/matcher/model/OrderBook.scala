package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.annotation.tailrec
import scala.collection.mutable

class OrderBook private (private[OrderBook] val bids: OrderBook.Side, private[OrderBook] val asks: OrderBook.Side) {
  import OrderBook._

  def bestBid: Option[LevelAgg] = bids.aggregated.headOption
  def bestAsk: Option[LevelAgg] = asks.aggregated.headOption

  def allOrders: Iterable[LimitOrder] =
    for {
      (_, level) <- bids ++ asks
      lo         <- level
    } yield lo

  def cancel(orderId: ByteStr): Option[OrderCanceled] = ???

  def cancelAll(): Seq[OrderCanceled] = {
    val canceledOrders = allOrders.map(lo => OrderCanceled(lo, unmatchable = false)).toSeq
    bids.clear()
    asks.clear()
    canceledOrders
  }

  def add(o: Order, ts: Long): Seq[Event] =
    (o.orderType match {
      case OrderType.BUY  => doMatch(ts, _ >= _, LimitOrder(o), Seq.empty, bids, asks)
      case OrderType.SELL => doMatch(ts, _ <= _, LimitOrder(o), Seq.empty, asks, bids)
    }).reverse

  def snapshot: Snapshot = Snapshot(bids.aggregated.toSeq, asks.aggregated.toSeq)
}

object OrderBook {
  type Level = Vector[LimitOrder]
  type Side  = mutable.TreeMap[Price, Level]

  case class Snapshot(bids: Seq[LevelAgg] = Seq.empty, asks: Seq[LevelAgg] = Seq.empty)

  implicit class SideExt(val side: Side) extends AnyVal {
    def best: Option[LimitOrder] = side.headOption.flatMap(_._2.headOption)

    final def removeBest(): LimitOrder = side.headOption match {
      case l if l.forall(_._2.isEmpty) =>
        throw new IllegalArgumentException("Cannot remove the best element from an empty level")
      case Some((price, level)) =>
        if (level.length == 1) side -= price
        else side += price -> level.tail
        level.head
    }

    def replaceBest(newBestAsk: LimitOrder): Side = {
      require(side.nonEmpty, "Cannot replace the best level of an empty side")
      val (price, level) = side.head
      require(level.nonEmpty, "Cannot replace the best element of an empty level")
      side += (price -> (newBestAsk +: level.tail))
    }

    def aggregated: Iterable[LevelAgg] =
      for {
        (p, l) <- side.view
        if l.nonEmpty
      } yield LevelAgg(l.map(_.amount).sum, p)
  }

  /** @param canMatch (submittedPrice, counterPrice) => Boolean */
  @tailrec
  private def doMatch(
      eventTs: Long,
      canMatch: (Long, Long) => Boolean,
      submitted: LimitOrder,
      prevEvents: Seq[Event],
      submittedSide: Side,
      counterSide: Side,
  ): Seq[Event] = counterSide.best match {
    case counter if counter.forall(c => !canMatch(submitted.price, c.price)) =>
      submittedSide += submitted.price -> (submittedSide.getOrElse(submitted.price, Vector.empty) :+ submitted)
      Seq(OrderAdded(submitted))
    case Some(counter) =>
      if (!counter.order.isValid(eventTs)) {
        counterSide.removeBest()
        doMatch(eventTs, canMatch, submitted, prevEvents :+ OrderCanceled(counter, false), submittedSide, counterSide)
      } else {
        val x = OrderExecuted(submitted, counter)

        require(
          !(x.counterRemaining.isValid && x.submittedRemaining.isValid),
          s"Either submitted ${submitted.order.id()} or counter ${counter.order.id()} must match completely"
        )

        val newEvents = x +: prevEvents

        if (x.submittedRemaining.isValid) {
          counterSide.removeBest()
          doMatch(eventTs, canMatch, x.submittedRemaining, newEvents, submittedSide, counterSide)
        } else {
          if (x.counterRemaining.isValid) {
            counterSide.replaceBest(x.counterRemaining)
          } else {
            counterSide.removeBest()
          }

          newEvents
        }
      }
  }

  val bidsOrdering: Ordering[Long] = (x: Long, y: Long) => -Ordering.Long.compare(x, y)
  val asksOrdering: Ordering[Long] = (x: Long, y: Long) => Ordering.Long.compare(x, y)

  import com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat

  private implicit val limitOrderFormat: Format[LimitOrder] = (
    (JsPath \ "amount").format[Long] and
      (JsPath \ "fee").format[Long] and
      (JsPath \ "order").format[Order]
  )(LimitOrder.limitOrder, lo => (lo.amount, lo.fee, lo.order))

  private type SideJson = Map[Price, Seq[LimitOrder]]

  implicit val priceMapFormat: Format[SideJson] =
    implicitly[Format[Map[String, Seq[LimitOrder]]]].inmap(
      _.map { case (k, v) => k.toLong   -> v },
      _.map { case (k, v) => k.toString -> v }
    )

  implicit val orderBookFormat: Format[OrderBook] = (
    (JsPath \ "bids").format[SideJson] and
      (JsPath \ "asks").format[SideJson]
  )(apply, unapply)

  def empty: OrderBook = new OrderBook(mutable.TreeMap.empty(bidsOrdering), mutable.TreeMap.empty(asksOrdering))

  private def unapply(ob: OrderBook): (Map[Price, Level], Map[Price, Level]) = (ob.asks.toMap, ob.asks.toMap)

  private def transformSide(side: SideJson, expectedSide: OrderType): Side = {
    val bidMap = mutable.TreeMap.empty[Price, Level](bidsOrdering)
    for ((p, level) <- side) {
      val v = Vector.newBuilder[LimitOrder]
      for (lo <- level) {
        require(lo.order.orderType == expectedSide,
                s"Expecting $expectedSide only orders in bid list, but ${lo.order.id()} is a ${lo.order.orderType} order")
        v += lo
      }
      bidMap += p -> v.result()
    }
    bidMap
  }

  private def apply(bids: SideJson, asks: SideJson): OrderBook =
    new OrderBook(transformSide(bids, OrderType.BUY), transformSide(asks, OrderType.BUY))
}
