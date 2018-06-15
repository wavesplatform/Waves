package com.wavesplatform.lang.v1.traits

trait Environment {
  def height: Int
  def networkByte: Byte
  def transaction: Tx
  def transactionById(id: Array[Byte]): Option[Tx]
  def transactionHeightById(id: Array[Byte]): Option[Int]
  def data(addressOrAlias: Array[Byte], key: String, dataType: DataType): Option[Any]
  def resolveAlias(name: String): Either[String, Recipient.Address]
  def accountBalanceOf(addressOrAlias: Array[Byte], assetId: Option[Array[Byte]]): Either[String, Long]
}
