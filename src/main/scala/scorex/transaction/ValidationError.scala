package scorex.transaction

import com.wavesplatform.state2.ByteStr
import scorex.account.{Address, Alias}
import scorex.block.{Block, MicroBlock}
import scorex.transaction.assets.exchange.Order

trait ValidationError

object ValidationError {

  case class InvalidAddress(reason: String) extends ValidationError
  case class NegativeAmount(amount: Long, of: String) extends ValidationError
  case object InsufficientFee extends ValidationError
  case object TooBigArray extends ValidationError
  case object InvalidName extends ValidationError
  case object OverflowError extends ValidationError
  case object ToSelf extends ValidationError
  case object MissingSenderPrivateKey extends ValidationError
  case object UnsupportedTransactionType extends ValidationError
  case object InvalidRequestSignature extends ValidationError
  case class BlockFromFuture(ts: Long) extends ValidationError
  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${s.toString + " reason: " + details})"
  }
  case class GenericError(err: String) extends ValidationError
  case class AlreadyInThePool(txId: ByteStr) extends ValidationError
  case class AccountBalanceError(errs: Map[Address, String]) extends ValidationError
  case class AliasNotExists(a: Alias) extends ValidationError
  case class OrderValidationError(order: Order, err: String) extends ValidationError
  case class Mistiming(err: String) extends ValidationError
  case class BlockAppendError(err: String,b: Block)  extends ValidationError
  case class MicroBlockAppendError(err: String, microBlock: MicroBlock) extends ValidationError {
    override def toString: String = s"MicroBlockAppendError($err, ${microBlock.totalResBlockSig} ~> ${microBlock.prevResBlockSig.trim}])"
  }

  case class ActivationError(err: String) extends ValidationError
}
