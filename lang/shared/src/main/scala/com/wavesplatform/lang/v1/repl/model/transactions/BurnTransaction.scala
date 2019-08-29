package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Signable, Transaction, WithId}

object BurnTransaction {
  val BURN = 6
}

trait BurnTransaction extends Transaction with Signable with WithId {
  def assetId: String
  def amount: Long
}