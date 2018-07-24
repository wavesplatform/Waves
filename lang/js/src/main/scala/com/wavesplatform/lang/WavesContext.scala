package com.wavesplatform.lang

import com.wavesplatform.lang.v1.traits.{DataType, Environment, Recipient, Tx}

object WavesContext {
  val env = new Environment {
    override def height: Int                                         = impl.Environment.height
    override def networkByte: Byte                                   = impl.Environment.networkByte
    override def inputEntity: Tx                                     = impl.Environment.transaction
    override def transactionById(id: Array[Byte]): Option[Tx]        = impl.Environment.transactionById(id)
    override def transactionHeightById(id: Array[Byte]): Option[Int] = impl.Environment.transactionHeightById(id)
    override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] =
      impl.Environment.data(addressOrAlias, key, dataType)
    override def resolveAlias(name: String): Either[String, Recipient.Address] = impl.Environment.resolveAddress(name)
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] =
      impl.Environment.accountBalanceOf(addressOrAlias, assetId)
  }
}
