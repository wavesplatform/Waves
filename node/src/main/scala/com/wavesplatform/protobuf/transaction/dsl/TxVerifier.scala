package com.wavesplatform.protobuf.transaction.dsl

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{PBCachedTransaction, PBSignedTransaction}
import com.wavesplatform.protobuf.transaction.dsl.PBTransactionsDSL.Matchers
import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.TxValidationError

trait TxVerifier {
  def verify(tx: PBCachedTransaction): Either[ValidationError, Unit]
}

object TxVerifier {
  implicit object Default extends TxVerifier {
    override def verify(tx: PBCachedTransaction): Either[ValidationError, Unit] = tx match {
      case Matchers.Data(bf, dataEntries) =>
        val MaxBytes = 150 * 1024

        if (dataEntries.lengthCompare(MaxEntryCount) > 0 || dataEntries.exists(!_.valid)) {
          Left(TxValidationError.TooBigArray)
        } else if (dataEntries.exists(_.key.isEmpty)) {
          Left(TxValidationError.GenericError("Empty key found"))
        } else if (dataEntries.map(_.key).distinct.lengthCompare(dataEntries.size) < 0) {
          Left(TxValidationError.GenericError("Duplicate keys found"))
        } else if (tx.transaction.getTransaction.getFee.amount <= 0) {
          Left(TxValidationError.InsufficientFee())
        } else {
          Either.cond(tx.serializedSize <= MaxBytes, (), TxValidationError.TooBigArray)
        }

      case _ =>
        Left(TxValidationError.GenericError("todo"))
    }
  }
}
