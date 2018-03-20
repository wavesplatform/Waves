package com.wavesplatform.lang

import com.wavesplatform.lang.traits.{Emulator, Transaction}

object WavesContext extends WavesContextImpl with js.Crypto with Emulator {
  override def height: Int = ???

  override def transaction: Transaction = ???

  override def transactionById(id: Array[Byte]): Option[Transaction] = ???
}