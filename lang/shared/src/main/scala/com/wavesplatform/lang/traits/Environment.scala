package com.wavesplatform.lang.traits

trait Environment {
  def height: Int
  def networkByte : Byte
  def transaction: Transaction
  def transactionById(id: Array[Byte]): Option[Transaction]
  def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]]
}
