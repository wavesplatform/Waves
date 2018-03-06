package com.wavesplatform.lang.traits

trait Emulator {
  def height: Int
  def transaction: Transaction
  def transactionById(id: Array[Byte]): Option[Transaction]
}
