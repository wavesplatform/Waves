package com.wavesplatform.transaction.validation

trait TxValidator[T] {
  def validate(tx: T): ValidatedV[T]
}
