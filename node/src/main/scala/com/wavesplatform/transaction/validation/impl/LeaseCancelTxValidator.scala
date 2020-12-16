package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.validation.TxValidator

object LeaseCancelTxValidator extends TxValidator[LeaseCancelTransaction] {
  override def validate(tx: LeaseCancelTransaction): ValidatedNel[ValidationError, LeaseCancelTransaction] = {
    import tx._
    V.seq(tx)(
      V.fee(fee),
      checkLeaseId(leaseId).toValidatedNel
    )
  }

  def checkLeaseId(leaseId: ByteStr): Either[GenericError, Unit] =
    Either.cond(leaseId.arr.length == crypto.DigestLength, (), GenericError("Lease transaction id is invalid"))
}
