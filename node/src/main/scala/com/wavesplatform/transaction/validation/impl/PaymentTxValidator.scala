package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.PaymentTransaction
import com.wavesplatform.transaction.validation.TxValidator

object PaymentTxValidator extends TxValidator[PaymentTransaction] {
  override def validate(transaction: PaymentTransaction): ValidatedNel[ValidationError, PaymentTransaction] = {
    import transaction.*
    V.seq(transaction)(
      V.noOverflow(fee.value, amount.value),
      V.addressChainId(recipient, chainId)
    )
  }
}
