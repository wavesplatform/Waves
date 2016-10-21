package com.wavesplatform.matcher.market

import java.util
import java.util.Comparator

import scala.collection.JavaConversions._

import scorex.transaction.assets.exchange.{AssetPair, Order}

class OrderBook(val assetPair: AssetPair, val comparator: Comparator[Long]) {
  private val priceOrders: util.TreeMap[Long, Level] = new util.TreeMap[Long, Level](comparator)

  def getBestOrders: Option[Level] = {
    if (priceOrders.isEmpty) {
      None
    } else {
      Option(priceOrders.firstEntry.getValue)
    }
  }

  def add(order: Order) {
    priceOrders.putIfAbsent(order.price, new Level(assetPair, order.price))
    priceOrders.get(order.price) += order
  }

  def execute(order: Order): (Seq[Order], Long) = {
    getBestOrders match {
      case Some(bestOrders) =>
        if (comparator.compare(bestOrders.price, order.price) <= 0) {
          val (executed, remaining) = bestOrders.execute(order)

          if (bestOrders.isEmpty) delete(bestOrders)
          if (remaining > 0) {
            val (remainingExecuted, rest) = execute(order.copy(amount = remaining))
            (executed ++ remainingExecuted, rest)
          } else (executed, remaining)
        } else (Seq.empty, order.amount)
      case None => (Seq.empty, order.amount)
    }
  }

  def delete(orders: Level) {
    priceOrders.remove(orders.price)
  }

  def flattenOrders: Seq[Order] = {
    priceOrders.values().flatMap(orders => orders.orders).toSeq
  }

}
