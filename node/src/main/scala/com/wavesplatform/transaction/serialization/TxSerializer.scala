package com.wavesplatform.transaction.serialization

import com.wavesplatform.transaction.Transaction

trait TxSerializer[T <: Transaction] extends TxBytes[T] with TxJson[T]

object TxSerializer {
  def apply[T <: Transaction : TxSerializer]: TxSerializer[T] = implicitly[TxSerializer[T]]
}
