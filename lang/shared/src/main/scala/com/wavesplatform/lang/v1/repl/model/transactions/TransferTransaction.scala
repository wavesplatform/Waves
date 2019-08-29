package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Signable, Transaction, WithId}

object TransferTransaction {
  val TRANSFER = 4
}

trait TransferTransaction extends Transaction with Signable with WithId {
  def recipient: String
  def amount: Long
  def assetId: String
  def feeAssetId: String
  def attachment: ByteString
}