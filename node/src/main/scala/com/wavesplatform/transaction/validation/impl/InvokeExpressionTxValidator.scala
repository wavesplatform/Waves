package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object InvokeExpressionTxValidator extends TxValidator[InvokeExpressionTransaction] {
  override def validate(tx: InvokeExpressionTransaction): ValidatedV[InvokeExpressionTransaction] =
    V.seq(tx)(
      V.fee(tx.fee),
      V.invokeLength(tx.expressionBytes.size <= ContractLimits.MaxContractSizeInBytes),
      V.cond(tx.expression.isFreeCall, GenericError("Script type for Invoke Expression Transaction should be CALL"))
    )
}
