package com.wavesplatform.lang.v1.repl.model.transactions

object ReissueTransaction {
  val REISSUE = 5
}

trait ReissueTransaction extends Transaction with Signable with WithId {
  def getAssetId: String

  def getQuantity: Long

  def isReissuable: Boolean
}