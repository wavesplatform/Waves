package com.wavesplatform.transaction.validation.impl

import com.wavesplatform.protobuf.transaction.DataTransactionData
import com.wavesplatform.protobuf.transaction.PBTransactions.toPBDataEntry
import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}
import com.wavesplatform.transaction.{DataTransaction, TxValidationError}

import scala.util.Try

object DataTxValidator extends TxValidator[DataTransaction] {
  override def validate(tx: DataTransaction): ValidatedV[DataTransaction] = {
    import tx._

    lazy val validLength =
      if (tx.isProtobufVersion) DataTransactionData(data.map(toPBDataEntry)).toByteArray.length <= DataTransaction.MaxBytes
      else Try(bytes().length <= DataTransaction.MaxBytes).getOrElse(false)

    V.seq(tx)(
      V.cond(data.length <= MaxEntryCount && data.forall(_.valid), TxValidationError.TooBigArray),
      V.cond(data.forall(_.key.nonEmpty), TxValidationError.EmptyDataKey),
      V.cond(data.map(_.key) == data.map(_.key).distinct, TxValidationError.DuplicatedDataKeys),
      V.fee(fee),
      V.cond(validLength, TxValidationError.TooBigArray)
    )
  }
}
