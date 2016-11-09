package com.wavesplatform.matcher.model

import scorex.transaction.assets.exchange.Order

@SerialVersionUID(7331576534636053581L)
case class OrderItem(price: Long, amount: Long, order: Order) {

}

object OrderItem {
  def apply(o: Order): OrderItem = new OrderItem(o.price, o.amount, o)
}