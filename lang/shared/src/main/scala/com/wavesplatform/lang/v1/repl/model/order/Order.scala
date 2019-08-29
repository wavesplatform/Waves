package com.wavesplatform.lang.v1.repl.model.order

import com.wavesplatform.lang.v1.repl.model.transaction.ByteString
import com.wavesplatform.lang.v1.repl.model.{Account, AssetPair, OrderType, Signable}

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

object Order {
  val V1 = 1
  val V2 = 2
}
