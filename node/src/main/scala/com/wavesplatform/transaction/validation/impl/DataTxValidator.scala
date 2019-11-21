package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}
import com.wavesplatform.transaction.{DataTransaction, TxValidationError, TxVersion}

import scala.util.Try

object DataTxValidator extends TxValidator[DataTransaction] {
  override def validate(tx: DataTransaction): ValidatedV[DataTransaction] = {
    import tx._
    V.seq(tx)(
      V.cond(data.length <= MaxEntryCount && data.forall(_.isValid(version)), TxValidationError.TooBigArray),
      V.cond(data.forall(_.key.nonEmpty), TxValidationError.EmptyDataKey),
      V.cond(data.map(_.key) == data.map(_.key).distinct, TxValidationError.DuplicatedDataKeys),
      V.fee(fee),
      version match {
        case TxVersion.V1 =>
          V.cond(Try(bytes().length <= DataTransaction.MaxBytes).getOrElse(false), TxValidationError.TooBigArray)

        case _ =>
          V.cond(Try(protoDataPayload.length <= DataTransaction.MaxBytes).getOrElse(false), TxValidationError.TooBigArray)
      }
    )
  }
}
