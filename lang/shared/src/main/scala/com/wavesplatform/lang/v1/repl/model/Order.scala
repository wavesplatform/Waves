package com.wavesplatform.lang.v1.repl.model

import com.wavesplatform.lang.v1.repl.model.transactions.{ByteString, Signable}

trait Order extends Signable {
  def matcherPublicKey: Account
  def assetPair: AssetPair
  def orderType: OrderType
  def amount: Long
  def price: Long
  def timestamp: Long
  def expiration: Long
  def matcherFee: Long
  def proofs: List[ByteString]
  def version: Byte
}
