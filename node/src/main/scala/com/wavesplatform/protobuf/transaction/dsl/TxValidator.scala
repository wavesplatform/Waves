package com.wavesplatform.protobuf.transaction.dsl

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.{PBCachedTransaction, PBSignedTransaction, PBTransactions}
import com.wavesplatform.protobuf.transaction.dsl.PBTransactionsDSL.Matchers
import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.{DataTransaction, TxValidationError}

trait TxValidator {
  def validate(tx: PBCachedTransaction): Either[ValidationError, Unit]
}

object TxValidator {
  implicit object Default extends TxValidator {
    override def validate(tx: PBCachedTransaction): Either[ValidationError, Unit] = tx match {
      case Matchers.Data(bf, dataEntries) =>
        for {
          _ <- V.not(dataEntries.lengthCompare(MaxEntryCount) > 0 || dataEntries.exists(!_.valid), TxValidationError.TooBigArray)
          _ <- V.not(dataEntries.exists(_.key.isEmpty), TxValidationError.GenericError("Empty key found"))
          _ <- V.not(dataEntries.map(_.key).distinct.lengthCompare(dataEntries.size) < 0, TxValidationError.GenericError("Duplicate keys found"))
          _ <- V.hasFee(tx)
          _ <- V.bytesFits(tx, DataTransaction.MaxBytes)
        } yield ()

      case _ =>
        Left(TxValidationError.GenericError("todo"))
    }
  }

  private[this] object V {
    type Validation = Either[ValidationError, Unit]

    def is(f: => Boolean, ve: => ValidationError): Validation = Either.cond(f, (), ve)

    def not(f: => Boolean, ve: => ValidationError): Validation = Either.cond(!f, (), ve)

    def hasFee(tx: PBCachedTransaction): Validation =
      Either.cond(tx.transaction.getTransaction.getFee.amount <= 0, (), TxValidationError.InsufficientFee())

    def bytesFits(tx: PBCachedTransaction, maxSize: Int): Validation = {
      if (tx.transaction.getTransaction.version <= 2)
        PBTransactions.vanilla(tx, unsafe = true).filterOrElse(_.bytes().length <= maxSize, TxValidationError.TooBigArray).map(_ => ())
      else
        Either.cond(tx.serializedSize <= maxSize, (), TxValidationError.TooBigArray)
    }
  }
}
