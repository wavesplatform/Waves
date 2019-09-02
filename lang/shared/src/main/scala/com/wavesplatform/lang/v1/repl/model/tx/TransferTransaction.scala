package com.wavesplatform.lang.v1.repl.model.tx

import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, Signable, Transaction, WithId}

object TransferTransaction {
  val TRANSFER = 4
}

trait TransferTransaction {
  def id: ByteString
  def recipient: String
  def amount: Long
  def assetId: Option[String]
  def feeAssetId: Option[String]
  def attachment: ByteString
  def fee: Long
  def timestamp: Long
  def height: Int
  def `type`: Byte
  def version: Byte
  def senderPublicKey: Account
  def proofs: List[ByteString]
}