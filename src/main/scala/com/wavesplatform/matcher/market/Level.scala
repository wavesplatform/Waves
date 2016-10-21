package com.wavesplatform.matcher.market

import scorex.transaction.assets.exchange.{AssetPair, Order}

class Level(val assetPair: AssetPair, val price: Long) {
  var orders = Vector.empty[Order]

  def += (order: Order) {
    require(order.price == price && order.assetPair == assetPair)
    orders = orders :+ order
  }

  def execute(order: Order): (Seq[Order], Long) = {
    var remainingAmount = order.amount

    var (executed, rest) = orders.span { placedOrder =>
      if (placedOrder.amount <= remainingAmount) {
        remainingAmount -= placedOrder.amount
        true
      } else {
        false
      }
    }

    rest match {
      case partOrder +: others if remainingAmount > 0 =>
        executed = executed :+ partOrder.copy(amount = remainingAmount)
        rest = partOrder.copy(amount = partOrder.amount - remainingAmount) +: others
        remainingAmount = 0
      case _ =>
    }

    orders = rest
    (executed, remainingAmount)
  }

  def isEmpty: Boolean = orders.isEmpty
}
