package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import cats.syntax.either.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.validation.TxValidator

object LeaseCancelTxValidator extends TxValidator[LeaseCancelTransaction] {
  override def validate(tx: LeaseCancelTransaction): ValidatedNel[ValidationError, LeaseCancelTransaction] = {
    import tx.*
    V.seq(tx)(
      checkLeaseId(leaseId).toValidatedNel
    )
  }

  def checkLeaseId(leaseId: ByteStr): Either[GenericError, Unit] =
    Either.cond(
      leaseId.arr.length == crypto.DigestLength,
      (),
      GenericError(s"Lease id=$leaseId has invalid length = ${leaseId.arr.length} byte(s) while expecting ${crypto.DigestLength}")
    )
}
