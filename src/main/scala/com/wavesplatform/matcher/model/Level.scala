package com.wavesplatform.matcher.model

import scorex.transaction.assets.exchange.{AssetPair, Order}

case class LevelAgg(price: Long, amount: Long)

case class Level(price: Long, orders: Vector[OrderItem] = Vector.empty) {

  def execute(order: OrderItem): (Seq[OrderItem], Long) = {
    var remainingAmount = order.amount

    val (executed, rest) = orders.span { placedOrder =>
      if (placedOrder.amount <= remainingAmount) {
        remainingAmount -= placedOrder.amount
        true
      } else {
        false
      }
    }

    rest match {
      case partOrder +: others if remainingAmount > 0 =>
        (executed :+ partOrder.copy(amount = remainingAmount), 0)
      case _ => (executed, remainingAmount)
    }
  }

  def isEmpty: Boolean = orders.isEmpty

  def getAgg: LevelAgg = {
    LevelAgg(price, orders.foldLeft(0L)((b, o) => b + o.amount))
  }
}
