package scorex.transaction.state.database.blockchain

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Account, Alias}
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.{AliasNotExists, TransactionValidationError}
import scorex.transaction._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.state.extension.ExchangeTransactionValidator
import scorex.transaction.state.database.state.{AccState, ReasonIds}

import scala.compat.Platform.EOL
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Left, Right, Try}

trait Validator {
  def validate(trans: Seq[Transaction], heightOpt: Option[Int] = None, blockTime: Long): (Seq[ValidationError], Seq[Transaction])
}

class ValidatorImpl(s: StateReader, settings: FunctionalitySettings) extends Validator {
  override def validate(trans: Seq[Transaction], heightOpt: Option[Int], blockTime: Long): (Seq[ValidationError], Seq[Transaction]) = ???
}

object Validator {

  implicit class ValidatorExt(v: Validator) {

    def validate[T <: Transaction](tx: T, blockTime: Long): Either[ValidationError, T] = v.validate(Seq(tx), None, blockTime) match {
      case (_, Seq(t)) => Right(t.asInstanceOf[T])
      case (Seq(err), _) => Left(err)
    }

    // utility calls from test only

    def isValid(tx: Transaction, blockTime: Long): Boolean = validate(tx, blockTime).isRight

    def isValid(txs: Seq[Transaction], height: Option[Int] = None, blockTime: Long): Boolean = v.validate(txs, height, blockTime)._2.size == txs.size

  }

  implicit class SeqEitherHelper[L, R](eis: Seq[Either[L, R]]) {
    def segregate(): (Seq[L], Seq[R]) = (eis.filter(_.isLeft).map(_.left.get),
      eis.filter(_.isRight).map(_.right.get))
  }

  def safeSum(first: Long, second: Long): Try[Long] = Try {
    Math.addExact(first, second)
  }

}
