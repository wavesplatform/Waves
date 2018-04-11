package com.wavesplatform.lang.traits

trait Environment {
  def height: Int
  def networkByte: Byte
  def transaction: Transaction
  def transactionById(id: Array[Byte]): Option[Transaction]
  def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any]
  def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]]
  def accountBalanceOf(addressOrAlias: Array[Byte], assetId: Option[Array[Byte]]): Either[String, Long]
}
