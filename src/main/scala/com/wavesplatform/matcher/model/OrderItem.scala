package com.wavesplatform.matcher.model

import scorex.transaction.assets.exchange.{Order, OrderType}

@SerialVersionUID(7331576534636053581L)
case class OrderItem(price: Long, amount: Long, order: Order) {
  def sellAmount(): Long = {
    if (order.orderType == OrderType.SELL) amount
    else (BigInt(amount) * Order.PriceConstant / price).longValue()
  }

  def buyAmount(): Long = {
    if (order.orderType == OrderType.BUY) amount
    else (BigInt(amount) * Order.PriceConstant / price).longValue()
  }
}

object OrderItem {
  def apply(o: Order): OrderItem = new OrderItem(o.price, o.amount, o)
}