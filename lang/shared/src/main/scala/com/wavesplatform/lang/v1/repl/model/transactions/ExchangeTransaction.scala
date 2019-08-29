package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Order, Signable, Transaction, WithId}

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