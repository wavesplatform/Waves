package com.wavesplatform.transaction.validation.impl

import cats.syntax.either.*
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object InvokeExpressionTxValidator extends TxValidator[InvokeExpressionTransaction] {
  override def validate(tx: InvokeExpressionTransaction): ValidatedV[InvokeExpressionTransaction] = {
    val size  = tx.expressionBytes.size
    val limit = ContractLimits.MaxContractSizeInBytes
    V.seq(tx)(
      Either
        .cond(
          size <= limit,
          (),
          GenericError(s"InvokeExpressionTransaction bytes length = $size exceeds limit = $limit")
        )
        .toValidatedNel
    )
  }
}
