package scorex.transaction.state.database.blockchain

import cats._
import cats.implicits._
import cats.syntax.all._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.diffs.TransactionDiffer
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.transaction._

import scala.util.{Left, Right, Try}

trait Validator {
  def validate(trans: Seq[Transaction], heightOpt: Option[Int] = None, blockTime: Long): (Seq[ValidationError], Seq[Transaction])
}

class ValidatorImpl(s: StateReader, settings: FunctionalitySettings) extends Validator {
  override def validate(trans: Seq[Transaction], heightOpt: Option[Int], blockTime: Long): (Seq[ValidationError], Seq[Transaction]) = {
    val (errs, txs, _) = trans.foldLeft((Seq.empty[ValidationError], Seq.empty[Transaction], Monoid[Diff].empty)) {
      case ((errors, valid, diff), tx) =>
        TransactionDiffer.apply(settings, blockTime, heightOpt.getOrElse(s.height))(new CompositeStateReader(s, diff.asBlockDiff), tx) match {
          case Left(err) => (err +: errors, valid, diff)
          case Right(newDiff) => (errors, tx +: valid, Monoid[Diff].combine(diff, newDiff))
        }
    }
    (errs.reverse, txs.reverse)
  }
}

object Validator {

  implicit class ValidatorExt(v: Validator) {

    def validate[T <: Transaction](tx: T, blockTime: Long): Either[ValidationError, T] = v.validate(Seq(tx), None, blockTime) match {
      case (_, Seq(t)) => Right(t.asInstanceOf[T])
      case (Seq(err), _) => Left(err)
    }
  }

}
