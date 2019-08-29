package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Signable, Transaction, WithId}

object LeaseTransaction {
  val LEASE = 8
}

trait LeaseTransaction extends Transaction with Signable with WithId {
  def recipient: String
  def amount: Long
  def fee: Long
  def timestamp: Long
}