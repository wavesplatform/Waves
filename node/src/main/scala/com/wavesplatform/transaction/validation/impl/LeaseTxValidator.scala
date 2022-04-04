package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.TxValidationError

object LeaseTxValidator extends TxValidator[LeaseTransaction] {
  override def validate(tx: LeaseTransaction): ValidatedNel[ValidationError, LeaseTransaction] = {
    import tx._
    V.seq(tx)(
      V.noOverflow(amount.value, fee.value),
      V.cond(sender.toAddress != recipient, TxValidationError.ToSelf),
      V.addressChainId(recipient, chainId)
    )
  }

  def validateAmount(amount: Long) =
    Either.cond(amount > 0, (), TxValidationError.NonPositiveAmount(amount, "waves"))
}
