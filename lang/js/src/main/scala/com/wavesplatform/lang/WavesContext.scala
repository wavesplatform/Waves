package com.wavesplatform.lang

import com.wavesplatform.lang.js.Base58
import com.wavesplatform.lang.traits.{DataType, Environment, Transaction}

object WavesContext extends WavesContextImpl with js.Crypto with Environment with Base58 {
  override def height: Int = ???

  override def networkByte: Byte = ???

  override def transaction: Transaction = ???

  override def transactionById(id: Array[Byte]): Option[Transaction] = ???

  override def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any] = ???

  override def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]] = ???
}