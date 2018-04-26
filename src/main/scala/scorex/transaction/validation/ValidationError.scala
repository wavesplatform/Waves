package scorex.transaction.validation

import com.google.common.base.Throwables
import com.wavesplatform.state.ByteStr
import scorex.account.{Address, Alias}
import scorex.block.{Block, MicroBlock}
import scorex.transaction.assets.exchange.Order
import scorex.transaction.{Signed, Transaction}

import scala.util.Either

trait ValidationError

object ValidationError {
  type Validation[T] = Either[ValidationError, T]

  case class InvalidAddress(reason: String) extends ValidationError

  case class NegativeAmount(amount: Long, of: String) extends ValidationError

  case class NegativeMinFee(minFee: Long, of: String) extends ValidationError

  case class InsufficientFee(msg: String = "insufficient fee") extends ValidationError

  case object TooBigArray extends ValidationError

  case object InvalidName extends ValidationError

  case object NegativeTimestamp extends ValidationError

  case object OverflowError extends ValidationError

  case object ToSelf extends ValidationError

  case object MissingSenderPrivateKey extends ValidationError

  case object UnsupportedTransactionType extends ValidationError

  case object InvalidRequestSignature extends ValidationError

  case class BlockFromFuture(ts: Long) extends ValidationError

  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${s.toString + " reason: " + details})"
  }

  case class TransactionNotAllowedByScript(t: Transaction) extends ValidationError {
    override def toString: String = s"TransactionNotAllowedByScript($t)"
  }

  case class ScriptParseError(m: String) extends ValidationError

  case class GenericError(err: String) extends ValidationError

  object GenericError {
    def apply(ex: Throwable): GenericError = new GenericError(Throwables.getStackTraceAsString(ex))
  }

  case class AlreadyInTheState(txId: ByteStr, txHeight: Int) extends ValidationError

  case class AccountBalanceError(errs: Map[Address, String]) extends ValidationError

  case class AliasDoesNotExist(a: Alias) extends ValidationError

  case class OrderValidationError(order: Order, err: String) extends ValidationError

  case class SenderIsBlacklisted(addr: String) extends ValidationError

  case class Mistiming(err: String) extends ValidationError

  case class BlockAppendError(err: String, b: Block) extends ValidationError

  case class MicroBlockAppendError(err: String, microBlock: MicroBlock) extends ValidationError {
    override def toString: String = s"MicroBlockAppendError($err, ${microBlock.totalResBlockSig} ~> ${microBlock.prevResBlockSig.trim}])"
  }

  case class ActivationError(err: String) extends ValidationError

  case class UnsupportedVersion(version: Int) extends ValidationError
}
