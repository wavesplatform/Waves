package com.wavesplatform.protobuf.transaction.dsl

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.dsl.PBTransactionsDSL.Matchers
import com.wavesplatform.protobuf.transaction.{PBCachedTransaction, PBTransactions}
import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.{DataTransaction, TxValidationError}

trait TxValidator {
  def validate(tx: PBCachedTransaction): TxValidator.ValidationsList
}

object TxValidator {
  type Validation = Validated[ValidationError, Unit]
  type ValidationsList = ValidatedNel[ValidationError, Unit]

  implicit object Default extends TxValidator {
    override def validate(tx: PBCachedTransaction): ValidationsList = tx match {
      case Matchers.Data(_, dataEntries) =>
        V.seq(
          V.not(dataEntries.lengthCompare(MaxEntryCount) > 0 || dataEntries.exists(!_.valid), TxValidationError.TooBigArray),
          V.not(dataEntries.exists(_.key.isEmpty), TxValidationError.GenericError("Empty key found")),
          V.not(dataEntries.map(_.key).distinct.lengthCompare(dataEntries.size) < 0, TxValidationError.GenericError("Duplicate keys found")),
          V.hasFee(tx),
          V.bytesFits(tx, DataTransaction.MaxBytes)
        )

      case _ =>
        Validated.invalidNel(TxValidationError.GenericError("todo"))
    }
  }

  private[this] object V {
    def is(condition: => Boolean, error: => ValidationError): Validation = Validated.cond(condition, (), error)

    def not(condition: => Boolean, error: => ValidationError): Validation = Validated.cond(!condition, (), error)

    def hasFee(tx: PBCachedTransaction): Validation =
      Validated.cond(tx.transaction.getTransaction.getFee.amount > 0, (), TxValidationError.InsufficientFee())

    def bytesFits(tx: PBCachedTransaction, maxSize: Int): Validation = Validated.fromEither {
      if (tx.transaction.getTransaction.version <= 2)
        PBTransactions.vanilla(tx, unsafe = true).filterOrElse(_.bytes().length <= maxSize, TxValidationError.TooBigArray).map(_ => ())
      else
        Either.cond(tx.serializedSize <= maxSize, (), TxValidationError.TooBigArray)
    }

    def seq(vs: Validation*): ValidationsList = {
      import cats.implicits._
      vs.map(_.toValidatedNel).fold(Validated.validNel(()))(_ combine _)
    }
  }
}
