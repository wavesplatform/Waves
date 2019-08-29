package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Alias, Signable, Transaction, WithId}

object AliasTransaction {
  val ALIAS = 10
}

trait AliasTransaction extends Transaction with Signable with WithId {
  def alias: Alias
}