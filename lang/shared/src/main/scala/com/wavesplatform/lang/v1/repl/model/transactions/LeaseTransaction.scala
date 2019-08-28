package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.WithId

object LeaseTransaction {
  val LEASE = 8
}

trait LeaseTransaction extends Transaction with Signable with WithId {
  def recipient: String
  def amount: Long
  def fee: Long
  def timestamp: Long
}