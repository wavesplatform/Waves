package com.wavesplatform.matcher.model

import scorex.transaction.assets.exchange.Order

case class OrderItem(price: Long, amount: Long, order: Order) {

}

object OrderItem {
  def apply(o: Order): OrderItem = new OrderItem(o.price, o.amount, o)
}