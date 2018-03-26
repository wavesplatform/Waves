package com.wavesplatform.lang

import com.wavesplatform.lang.traits.{Environment, Transaction}

object WavesContext extends WavesContextImpl with js.Crypto with Environment {
  override def height: Int = ???

  override def transaction: Transaction = ???

  override def transactionById(id: Array[Byte]): Option[Transaction] = ???

  override def networkByte: Byte = ???

  override def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]] = ???
}