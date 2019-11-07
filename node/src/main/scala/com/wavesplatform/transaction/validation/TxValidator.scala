package com.wavesplatform.transaction.validation

import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Transaction

trait TxValidator[T <: Transaction] {
  def validate(tx: T): ValidatedNel[ValidationError, T]
}
