package com.wavesplatform.lang.v1.repl.model.transactions

object TransferTransaction {
  val TRANSFER = 4
}

trait TransferTransaction extends Transaction with Signable with WithId {
  def getRecipient: String

  def getAmount: Long

  def getAssetId: String

  def getFeeAssetId: String

  def getAttachment: ByteString
}