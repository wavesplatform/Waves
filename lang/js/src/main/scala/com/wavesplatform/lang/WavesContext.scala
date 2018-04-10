package com.wavesplatform.lang

import com.wavesplatform.lang.traits.{DataType, Environment, Transaction}

object WavesContext {
  val env = new Environment {
    override def height: Int                                                                   = impl.Environment.height
    override def networkByte: Byte                                                             = impl.Environment.networkByte
    override def transaction: Transaction                                                      = impl.Environment.transaction
    override def transactionById(id: Array[Byte]): Option[Transaction]                         = impl.Environment.transactionById(id)
    override def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any] = impl.Environment.data(addressBytes, key, dataType)
    override def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]]      = impl.Environment.resolveAddress(addressOrAlias)
  }
}
