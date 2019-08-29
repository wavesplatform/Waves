package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Signable, Transaction, WithId}

object ReissueTransaction {
  val REISSUE = 5
}

trait ReissueTransaction extends Transaction with Signable with WithId {
  def assetId: String
  def quantity: Long
  def isReissuable: Boolean
}