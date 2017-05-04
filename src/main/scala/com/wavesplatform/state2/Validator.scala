package com.wavesplatform.state2

import cats._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.TransactionDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction._
import scala.concurrent.duration._
import scala.util.{Left, Right}

object Validator {

  val MaxTimePreviousBlockOverTransactionDiff: FiniteDuration = 90.minutes

  def validate(settings: FunctionalitySettings, s: StateReader,
               trans: Seq[Transaction], heightOpt: Option[Int], blockTime: Long): (Seq[ValidationError], Seq[Transaction]) = {
    val (errs, txs, _) = trans.foldLeft((Seq.empty[ValidationError], Seq.empty[Transaction], Monoid[Diff].empty)) {
      case ((errors, valid, diff), tx) =>
        TransactionDiffer.apply(settings, blockTime, heightOpt.getOrElse(s.height))(new CompositeStateReader(s, diff.asBlockDiff), tx) match {
          case Left(err) => (err +: errors, valid, diff)
          case Right(newDiff) => (errors, tx +: valid, Monoid[Diff].combine(diff, newDiff))
        }
    }
    (errs.reverse, txs.reverse)
  }

  def validateWithHistory[T <: Transaction](history: History, fs: FunctionalitySettings, stateReader: StateReader)(tx: T): Either[ValidationError, T] = {
    val lastBlockTimestamp = history.lastBlock.timestamp
    val notExpired = (lastBlockTimestamp - tx.timestamp).millis <= MaxTimePreviousBlockOverTransactionDiff
    if (notExpired) {
      validate(fs, stateReader, Seq(tx), None, tx.timestamp) match {
        case (_, Seq(t)) => Right(t.asInstanceOf[T])
        case (Seq(err), _) => Left(err)
      }
    } else {
      Left(TransactionValidationError(tx, s"Transaction is too old: Last block timestamp is $lastBlockTimestamp"))
    }
  }


}
