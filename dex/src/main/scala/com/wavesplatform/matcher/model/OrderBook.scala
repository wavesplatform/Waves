package com.wavesplatform.matcher.model

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.MatcherModel.Price
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import play.api.libs.functional.syntax._
import play.api.libs.json._

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

  def cancel(orderId: ByteStr): Option[OrderCanceled] = {
    allOrders.collectFirst {
      case lo if lo.order.id() == orderId =>
        (if (lo.order.orderType == OrderType.BUY) bids else asks).remove(lo.order.price, lo.order.id())
        OrderCanceled(lo, false)
    }
  }

  def cancelAll(): Seq[OrderCanceled] = {
    val canceledOrders = allOrders.map(lo => OrderCanceled(lo, unmatchable = false)).toSeq
    bids.clear()
    asks.clear()
    canceledOrders
  }

  def add(o: Order, ts: Long): Seq[Event] =
    (o.orderType match {
      case OrderType.BUY  => doMatch(ts, buy, LimitOrder(o), Seq.empty, bids, asks)
      case OrderType.SELL => doMatch(ts, sell, LimitOrder(o), Seq.empty, asks, bids)
    }).reverse

  def snapshot: Snapshot                     = Snapshot(bids.toMap, asks.toMap)
  def aggregatedSnapshot: AggregatedSnapshot = AggregatedSnapshot(bids.aggregated.toSeq, asks.aggregated.toSeq)

  override def toString = s"""{"bids":${formatSide(bids)},"asks":${formatSide(asks)}}"""
}

object OrderBook {
  type Level        = Vector[LimitOrder]
  type Side         = mutable.TreeMap[Price, Level]
  type SideSnapshot = Map[Price, Seq[LimitOrder]]

  case class Snapshot(bids: SideSnapshot, asks: SideSnapshot)
  case class AggregatedSnapshot(bids: Seq[LevelAgg] = Seq.empty, asks: Seq[LevelAgg] = Seq.empty)

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

    def remove(price: Price, orderId: ByteStr): LimitOrder = {
      val (toRemove, toKeep) = side.getOrElse(price, Vector.empty).partition(_.order.id() == orderId)
      require(toRemove.nonEmpty, s"Order $orderId not found at $price")
      if (toKeep.isEmpty) side -= price else side += price -> toKeep

      toRemove.head
    }

    def aggregated: Iterable[LevelAgg] =
      for {
        (p, l) <- side.view
        if l.nonEmpty
      } yield LevelAgg(l.map(_.amount).sum, p)
  }

  private object buy extends ((Long, Long) => Boolean) {
    def apply(submittedPrice: Long, counterPrice: Long): Boolean = submittedPrice >= counterPrice
    override val toString                                        = "submitted >= counter"
  }

  private object sell extends ((Long, Long) => Boolean) {
    def apply(submittedPrice: Long, counterPrice: Long): Boolean = submittedPrice <= counterPrice
    override val toString                                        = "submitted <= counter"
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
  ): Seq[Event] =
    if (!submitted.order.isValid(eventTs)) OrderCanceled(submitted, false) +: prevEvents
    else
      counterSide.best match {
        case counter if counter.forall(c => !canMatch(submitted.price, c.price)) =>
          submittedSide += submitted.price -> (submittedSide.getOrElse(submitted.price, Vector.empty) :+ submitted)
          OrderAdded(submitted) +: prevEvents
        case Some(counter) =>
          if (!submitted.isValid(counter.price)) {
            OrderCanceled(submitted, true) +: prevEvents
          } else if (!counter.order.isValid(eventTs)) {
            counterSide.removeBest()
            doMatch(eventTs, canMatch, submitted, OrderCanceled(counter, false) +: prevEvents, submittedSide, counterSide)
          } else {
            val x         = OrderExecuted(submitted, counter, eventTs)
            val newEvents = x +: prevEvents

            if (x.counterRemaining.isValid) {
              counterSide.replaceBest(x.counterRemaining)
              if (x.submittedRemaining.isValid) OrderCanceled(x.submittedRemaining, true) +: newEvents
              else newEvents
            } else {
              counterSide.removeBest()
              if (x.submittedRemaining.isValid) {
                doMatch(eventTs, canMatch, x.submittedRemaining, newEvents, submittedSide, counterSide)
              } else newEvents
            }
          }
      }

  private def formatSide(side: Side) =
    side
      .map { case (price, level) => s""""$price":${level.map(lo => s""""${lo.order.id()}"""").mkString("[", ",", "]")}""" }
      .mkString("{", ",", "}")

  val bidsOrdering: Ordering[Long] = (x: Long, y: Long) => -Ordering.Long.compare(x, y)
  val asksOrdering: Ordering[Long] = (x: Long, y: Long) => Ordering.Long.compare(x, y)

  import com.wavesplatform.transaction.assets.exchange.OrderJson.orderFormat

  private def limitOrder(remainingAmount: Long, remainingFee: Long, o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY  => BuyLimitOrder(remainingAmount, remainingFee, o)
    case OrderType.SELL => SellLimitOrder(remainingAmount, remainingFee, o)
  }

  private implicit val limitOrderFormat: Format[LimitOrder] = (
    (JsPath \ "amount").format[Long] and
      (JsPath \ "fee").format[Long] and
      (JsPath \ "order").format[Order]
  )(limitOrder, lo => (lo.amount, lo.fee, lo.order))

  implicit val priceMapFormat: Format[SideSnapshot] =
    implicitly[Format[Map[String, Seq[LimitOrder]]]].inmap(
      _.map { case (k, v) => k.toLong   -> v },
      _.map { case (k, v) => k.toString -> v }
    )

  implicit val snapshotFormat: Format[OrderBook.Snapshot] = Json.format

  def empty: OrderBook = new OrderBook(mutable.TreeMap.empty(bidsOrdering), mutable.TreeMap.empty(asksOrdering))

  private def transformSide(side: SideSnapshot, expectedSide: OrderType, ordering: Ordering[Long]): Side = {
    val bidMap = mutable.TreeMap.empty[Price, Level](ordering)
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

  def apply(snapshot: Snapshot): OrderBook =
    new OrderBook(transformSide(snapshot.bids, OrderType.BUY, bidsOrdering), transformSide(snapshot.asks, OrderType.SELL, asksOrdering))
}
