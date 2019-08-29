package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.order.Order
import com.wavesplatform.lang.v1.repl.model.{Signable, Transaction, WithId}

object ExchangeTransaction {
  val EXCHANGE = 7
}

trait ExchangeTransaction extends Transaction with Signable with WithId {
  def amount: Long
  def price: Long
  def buyMatcherFee: Long
  def sellMatcherFee: Long
  def order1: Order
  def order2: Order
}