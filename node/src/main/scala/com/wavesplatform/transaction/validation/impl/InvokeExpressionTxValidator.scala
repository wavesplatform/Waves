package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}

object InvokeExpressionTxValidator extends TxValidator[InvokeExpressionTransaction] {
  override def validate(tx: InvokeExpressionTransaction): ValidatedV[InvokeExpressionTransaction] =
    V.seq(tx)(
      V.invokeLength(tx.expressionBytes.size <= ContractLimits.MaxContractSizeInBytes)
    )
}
