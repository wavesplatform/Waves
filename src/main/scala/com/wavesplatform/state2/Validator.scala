package com.wavesplatform.state2

import cats._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.TransactionDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.transaction._

import scala.util.{Left, Right}

object Validator {

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

  def validate[T <: Transaction](settings: FunctionalitySettings, s: StateReader, tx: T): Either[ValidationError, T] =
    validate(settings, s, Seq(tx), None, tx.timestamp) match {
      case (_, Seq(t)) => Right(t.asInstanceOf[T])
      case (Seq(err), _) => Left(err)
    }

}
