package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.validation.TxValidator

object LeaseCancelTxValidator extends TxValidator[LeaseCancelTransaction] {
  override def validate(tx: LeaseCancelTransaction): ValidatedNel[ValidationError, LeaseCancelTransaction] = {
    import tx._
    Validations.seq(tx)(
      Validations.fee(fee),
      Validations.cond(leaseId.length == crypto.DigestLength, GenericError("Lease transaction id is invalid"))
    )
  }
}
