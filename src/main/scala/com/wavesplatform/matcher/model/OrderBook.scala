package com.wavesplatform.matcher.model

import java.util
import java.util.Comparator

import scorex.transaction.assets.exchange.{AssetPair, Order}

import scala.collection.JavaConversions._

class OrderBook(val assetPair: AssetPair, val comparator: Comparator[Long]) extends Serializable{
  val priceOrders: util.TreeMap[Long, Level] = new util.TreeMap[Long, Level](comparator)
  val ordersRemainingAmount = Map.empty[String, Long]

  def getBestOrders: Option[Level] = {
    if (priceOrders.isEmpty) {
      None
    } else {
      Option(priceOrders.firstEntry.getValue)
    }
  }

  def add(order: OrderItem) {
    val prevLevel = priceOrders.getOrDefault(order.price, Level(order.price))
    priceOrders.put(order.price, prevLevel.copy(orders = prevLevel.orders :+ order))
  }

  def doMatching(order: OrderItem, n: Int = 1): (Seq[OrderItem], Long) = {
    val bestOrders = priceOrders.keys.take(n)
    if (bestOrders.size == n) {
      val bestLevel = priceOrders.get(bestOrders.last)
      if (comparator.compare(bestLevel.price, order.price) <= 0) {
        val (executed, remaining) = bestLevel.execute(order)

        if (remaining > 0) {
          val (remainingExecuted, nextRemaining) = doMatching(order.copy(amount = remaining), n + 1)
          (executed ++ remainingExecuted, nextRemaining)
        } else (executed, remaining)
      } else (Seq.empty, order.amount)
    } else {
      (Seq.empty, order.amount)
    }
  }

  def removeOrder(order: OrderItem): Unit = {
    val prevLevel = priceOrders.getOrDefault(order.price, Level(order.price))

    val (before, after) = prevLevel.orders.span(_.order != order.order)
    if (after.nonEmpty) {
      val prevOrder = after.head
      val newOrders = if (order.amount < prevOrder.amount) {
        (before :+ prevOrder.copy(amount = prevOrder.amount - order.amount)) ++ after.tail
      } else before ++ after.tail

      if (newOrders.isEmpty) priceOrders.remove(order.price)
      else priceOrders.put(order.price, prevLevel.copy(orders = newOrders))
    }
  }

  def removeMatched(orders: Seq[OrderItem]) = {
    orders.foreach { o =>
      removeOrder(o)
    }
  }

  private def delete(orders: Level) {
    priceOrders.remove(orders.price)
  }

  def flattenOrders: Seq[OrderItem] = {
    priceOrders.values().flatMap(orders => orders.orders).toSeq
  }

  def take(depth: Int): Seq[LevelAgg] = {
    priceOrders.take(depth).values.map(_.getAgg).toList
  }

}
