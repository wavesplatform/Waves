package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Signable, Transaction, WithId}

object LeaseCancelTransaction {
  val LEASE_CANCEL = 9
}

trait LeaseCancelTransaction extends Transaction with Signable with WithId {
  def leaseId: String
}