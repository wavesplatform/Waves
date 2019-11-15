package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}
import com.wavesplatform.transaction.{DataTransaction, TxValidationError}

object DataTxValidator extends TxValidator[DataTransaction] {
  override def validate(tx: DataTransaction): ValidatedV[DataTransaction] = {
    import tx._
    V.seq(tx)(
      V.cond(data.length <= MaxEntryCount && data.forall(_.valid), TxValidationError.TooBigArray),
      V.cond(data.forall(_.key.nonEmpty), TxValidationError.EmptyDataKey),
      V.cond(data.map(_.key) == data.map(_.key).distinct, TxValidationError.DuplicatedDataKeys),
      V.fee(fee),
      V.cond(bytes().length <= DataTransaction.MaxBytes, TxValidationError.TooBigArray)
    )
  }
}
