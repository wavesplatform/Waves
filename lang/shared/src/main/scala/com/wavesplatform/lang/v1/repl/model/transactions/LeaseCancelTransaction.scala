package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.WithId

object LeaseCancelTransaction {
  val LEASE_CANCEL = 9
}

trait LeaseCancelTransaction extends Transaction with Signable with WithId {
  def leaseId: String
}